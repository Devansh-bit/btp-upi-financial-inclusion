import math
from pathlib import Path

import numpy as np
import pandas as pd

from btpdata.periods import quarters

from btpdata.cpi import (MAX_GAP_MONTHS, MIN_COVERAGE, apply_fallback, build, fill_gaps, link_bases, overlap_factors,
                         read_raw, to_quarterly)

FIX = Path(__file__).parent / "fixtures" / "cpi" / "cpi_sample.json"


def monthly(state, months, cpi, base="2012"):
    return pd.DataFrame({"state_id": state, "month": months, "cpi": cpi, "base": base})


def q(cpi):
    months = ["2018-01", "2018-02", "2018-03"][: len(cpi)]
    return to_quarterly(monthly("bihar", months, cpi)).cpi[0]


def test_quarter_mean():
    assert q(cpi=[100, 101, 102]) == 101.0


def test_quarter_needs_three_months():
    assert math.isnan(q(cpi=[100, 101]))


def test_link_no_jump():
    df = pd.concat([monthly("all_india", ["2025-12"], [180.0]),
                    monthly("all_india", ["2026-01"], [92.5], base="2024")])
    s = link_bases(df, {"all_india": 1.95})
    assert abs(s.cpi.iloc[-1] / 180.0 - 1) < 0.02


def test_overlap_factors():
    months = [f"2025-{m:02d}" for m in range(1, 13)]
    df = pd.concat([monthly("bihar", months, [200.0] * 12),
                    monthly("bihar", months, [100.0] * 12, base="2024"),
                    monthly("goa", months, [190.0] * 12),
                    monthly("goa", months, [95.0] * 12, base="2024")])
    f = overlap_factors(df)
    assert f == {"bihar": 2.0, "goa": 2.0}


def test_fixture_reads():
    df = read_raw(FIX)
    assert set(df.columns) == {"state_id", "month", "cpi", "base"}
    assert "ladakh" not in set(df.state_id) and "daman" not in " ".join(df.state_id)
    assert set(df.base) == {"2012", "2024"} and df.cpi.dtype == float
    assert df.month.str.match(r"\d{4}-\d{2}$").all()


def test_fallback():
    df = pd.concat([monthly("all_india", ["2018-01", "2018-02", "2018-03"], [100, 101, 102]),
                    monthly("bihar", ["2018-01", "2018-02", "2018-03"], [10, 11, 12])])
    out = apply_fallback(to_quarterly(df), ["bihar", "goa"])
    got = out.set_index("state_id")
    assert got.loc["goa", "cpi"] == 101.0 and got.loc["goa", "cpi_fallback"] == 1
    assert got.loc["bihar", "cpi"] == 11.0 and got.loc["bihar", "cpi_fallback"] == 0


def test_interior_gap_filled_and_counted():
    df = monthly("bihar", ["2018-01", "2018-04", "2018-05"], [100.0, 800.0, 900.0])
    out = to_quarterly(fill_gaps(df))
    row = out[out.period == "2018Q1"].iloc[0]
    # log-linear 100 -> 800 over 3 steps: 200, 400
    assert row.cpi_interpolated_months == 2
    assert math.isclose(row.cpi, (100 + 200 + 400) / 3, rel_tol=1e-9)


def test_gap_values_log_linear():
    df = monthly("bihar", ["2018-01", "2018-04"], [100.0, 800.0])
    f = fill_gaps(df).set_index("month").cpi
    assert np.isclose(f["2018-02"], 200.0) and np.isclose(f["2018-03"], 400.0)


def test_three_month_gap_filled_and_counted():
    assert MAX_GAP_MONTHS == 3
    months = ["2018-01", "2018-02", "2018-03", "2018-07"]
    out = to_quarterly(fill_gaps(monthly("bihar", months, [100.0, 100.0, 100.0, 1600.0])))
    row = out[out.period == "2018Q2"].iloc[0]  # log-linear: 200, 400, 800
    assert row.cpi_interpolated_months == 3
    assert math.isclose(row.cpi, (200 + 400 + 800) / 3, rel_tol=1e-9)


def test_four_month_gap_not_filled():
    months = ["2018-01", "2018-02", "2018-03", "2018-08"]
    f = fill_gaps(monthly("bihar", months, [100.0, 100.0, 100.0, 1600.0]))
    assert len(f) == 4
    out = apply_fallback(to_quarterly(f), ["bihar"], ["2018Q2"])  # no Q2 rows -> NaN
    assert math.isnan(out.cpi.iloc[0])


def test_low_coverage_uses_all_india_whole_period():
    assert MIN_COVERAGE == 0.9
    months = [f"2018-{m:02d}" for m in range(1, 13)]
    df = pd.concat([monthly("all_india", months, [float(100 + i) for i in range(12)]),
                    monthly("bihar", months[:6], [10.0] * 6)])
    wide, _ = build(df, ids=["bihar"], periods=quarters("2018Q1", "2018Q4"))
    assert (wide.cpi_fallback == 1).all()
    assert wide.cpi.tolist() == [101.0, 104.0, 107.0, 110.0]
