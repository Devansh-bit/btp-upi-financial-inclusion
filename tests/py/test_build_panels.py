import numpy as np
import pandas as pd
import pytest

from btpdata.build_panels import PanelCheckError, build_quarterly, check_panel, coverage
from btpdata.states import ALL_IDS, MAIN_SAMPLE

PERIODS = ["2018Q1", "2018Q2"]


def write_interim(d, overrides=None):
    """Write the three interim files for all ids and PERIODS. overrides: {(state, period, variable): value}."""
    overrides = overrides or {}
    base = {"phonepe_count": 100.0, "phonepe_amount_inr": 5000.0, "phonepe_users": 50.0,
            "population": 1000.0, "cpi": 100.0, "cpi_fallback": 0.0, "cpi_interpolated_months": 0.0}
    files = {"phonepe": ["phonepe_count", "phonepe_amount_inr", "phonepe_users"], "population": ["population"],
             "cpi": ["cpi", "cpi_fallback", "cpi_interpolated_months"]}
    for name, variables in files.items():
        rows = [(s, p, v, overrides.get((s, p, v), base[v]), "test")
                for s in ALL_IDS for p in PERIODS for v in variables]
        pd.DataFrame(rows, columns=["state_id", "period", "variable", "value", "source"]).to_csv(
            d / f"{name}.csv", index=False)
    return d


def test_merged_per_capita(tmp_path):
    d = write_interim(tmp_path, {("jammu_kashmir", "2018Q1", "phonepe_count"): 110.0,
                                 ("jammu_kashmir", "2018Q1", "population"): 1000.0})
    df = build_quarterly(d)
    row = df[(df.state_id == "jammu_kashmir") & (df.period == "2018Q1")].iloc[0]
    assert row.count_pc == pytest.approx(0.11)
    assert row.real_amount_pc == pytest.approx(5000 * 100 / 100 / 1000)
    assert row.users_per_1000 == pytest.approx(50.0)


def test_columns_and_order(tmp_path):
    df = build_quarterly(write_interim(tmp_path))
    assert list(df.columns) == [
        "state_id", "period", "in_main_sample", "phonepe_count", "phonepe_amount_inr", "phonepe_users",
        "population", "cpi", "cpi_fallback", "cpi_interpolated_months", "count_pc", "real_amount_pc",
        "users_per_1000", "ln_count_pc", "ln_real_amount_pc"]
    assert len(df) == len(ALL_IDS) * len(PERIODS)
    assert df.equals(df.sort_values(["state_id", "period"]).reset_index(drop=True))


def test_missing_file(tmp_path):
    d = write_interim(tmp_path)
    (d / "cpi.csv").unlink()
    with pytest.raises(FileNotFoundError):
        build_quarterly(d)


def test_duplicate(tmp_path):
    df = build_quarterly(write_interim(tmp_path))
    dup = pd.concat([df, df.iloc[[0]]], ignore_index=True)
    with pytest.raises(PanelCheckError, match="duplicate"):
        check_panel(dup)


def test_negative(tmp_path):
    df = build_quarterly(write_interim(tmp_path))
    df.loc[0, "count_pc"] = -1.0
    with pytest.raises(PanelCheckError, match="negative"):
        check_panel(df)


def test_outlier(tmp_path):
    d = write_interim(tmp_path, {("goa", "2018Q1", "phonepe_count"): 1100.0})
    df = build_quarterly(d)
    with pytest.raises(PanelCheckError, match=r"outlier.*goa.*2018Q1"):
        check_panel(df)


def test_clean_panel_passes(tmp_path):
    check_panel(build_quarterly(write_interim(tmp_path)))


def test_zero_is_nan_not_inf(tmp_path):
    d_zero = write_interim(tmp_path, {("goa", "2018Q1", "phonepe_count"): 0.0})
    df = build_quarterly(d_zero)
    assert df.ln_count_pc.isna().sum() == 1
    assert np.isfinite(df.ln_count_pc.dropna()).all()
    assert coverage(df).query("variable=='ln_count_pc'").n_missing.sum() == 1
    check_panel(df)


def test_missing_value_is_nan(tmp_path):
    d = write_interim(tmp_path, {("goa", "2018Q1", "population"): np.nan})
    df = build_quarterly(d)
    row = df[(df.state_id == "goa") & (df.period == "2018Q1")].iloc[0]
    assert np.isnan(row.count_pc) and np.isnan(row.ln_count_pc)
    check_panel(df)


def test_coverage_shape(tmp_path):
    cov = coverage(build_quarterly(write_interim(tmp_path)))
    assert list(cov.columns) == ["variable", "state_id", "n_missing"]
    assert set(cov.state_id) == set(ALL_IDS)
    assert "period" not in set(cov.variable) and "state_id" not in set(cov.variable)
    assert (cov.n_missing == 0).all()


def test_main_sample_flag(tmp_path):
    df = build_quarterly(write_interim(tmp_path))
    assert df.in_main_sample.sum() == len(MAIN_SAMPLE) * len(PERIODS)
