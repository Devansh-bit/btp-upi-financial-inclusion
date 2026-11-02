import json
from pathlib import Path

import pandas as pd
import pytest

from btpdata.npci import UNATTRIBUTED_LABELS, parse_month, read_raw, to_quarterly, unattributed_share
from btpdata.states import UnknownStateError

FIX = Path(__file__).parent / "fixtures" / "npci" / "npci_sample.json"
MONTHS = json.loads(FIX.read_text())["months"]


def resp(rows):
    return {"status": 200, "data": {"results": [
        {"state_union_territory": n, "district": d, "volume_in_mn": v, "value_in_cr": c} for n, d, v, c in rows]}}


def test_unattributed_labels():
    assert UNATTRIBUTED_LABELS == frozenset({"unclassified"})


def test_unattributed_dropped():
    df = parse_month(MONTHS["2025-Apr"], "2025-04")
    assert set(df.state_id) == {"maharashtra", "jammu_kashmir"}
    assert list(df.columns) == ["state_id", "month", "npci_volume", "npci_value_inr"]


def test_units_and_indian_grouping():
    df = parse_month(MONTHS["2025-Apr"], "2025-04").set_index("state_id")
    assert df.loc["maharashtra", "npci_volume"] == pytest.approx(2382.34e6)
    assert df.loc["maharashtra", "npci_value_inr"] == pytest.approx(278375.41e7)


def test_ladakh_summed_into_jammu_kashmir():
    df = parse_month(MONTHS["2025-Apr"], "2025-04").set_index("state_id")
    assert df.loc["jammu_kashmir", "npci_volume"] == pytest.approx((88.43 + 3.63) * 1e6)
    assert df.loc["jammu_kashmir", "npci_value_inr"] == pytest.approx((11544.77 + 651.93) * 1e7)


def test_dnh_dd_rows_summed():
    r = resp([("DADRA & NAGAR HAVELI", None, "1", "10"), ("DAMAN & DIU", None, "2", "20")])
    df = parse_month(r, "2025-04")
    assert df.state_id.tolist() == ["dnh_dd"]
    assert df.npci_volume[0] == 3e6 and df.npci_value_inr[0] == 30e7


def test_format_b_ignores_district_rows_and_strips_total():
    df = parse_month(MONTHS["2026-Mar"], "2026-03").set_index("state_id")
    assert set(df.index) == {"maharashtra", "karnataka"}
    assert df.loc["maharashtra", "npci_volume"] == pytest.approx(2263.24e6)


@pytest.mark.parametrize("marker", [None, "", "-"])
def test_state_row_district_markers(marker):
    r = resp([("MAHARASHTRA Total", marker, "30", "300"), ("MAHARASHTRA", "PUNE", "5", "50"),
              ("Unclassified #", marker, "10", "100")])
    assert parse_month(r, "2026-04").npci_volume.tolist() == [30e6]
    assert unattributed_share(r) == (0.25, 0.25)


def test_real_file_has_37_state_rows_each_month():
    raw = Path("data/raw/npci/npci_statewise_2025-04_2026-09.json")
    if not raw.exists():
        pytest.skip("raw NPCI file absent")
    df = read_raw(raw)
    assert df.groupby("month").state_id.nunique().between(34, 35).all()
    assert df.month.nunique() == 18


def test_unknown_row_fails():
    with pytest.raises(UnknownStateError):
        parse_month(resp([("ATLANTIS", None, "1", "1")]), "2025-04")


def test_unattributed_share():
    r = resp([("MAHARASHTRA", None, "30", "300"), ("Unclassified#", None, "10", "100")])
    assert unattributed_share(r) == (0.25, 0.25)
    vs, ws = unattributed_share(MONTHS["2026-Mar"])
    assert vs == pytest.approx(9902.66 / (2263.24 + 1181.05 + 9902.66))
    assert ws == pytest.approx(1139245.93 / (276685.31 + 171403.92 + 1139245.93))


def test_read_raw(tmp_path):
    df = read_raw(FIX)
    assert sorted(df.month.unique()) == ["2025-04", "2026-03"]


def monthly(months, vol=1.0):
    return pd.DataFrame({"state_id": "bihar", "month": months, "npci_volume": vol, "npci_value_inr": 2 * vol})


def test_quarter_sums_three_months():
    q = to_quarterly(monthly(["2025-04", "2025-05", "2025-06"]))
    assert q.period.tolist() == ["2025Q2"] and q.npci_volume[0] == 3.0 and q.npci_value_inr[0] == 6.0


def test_partial_quarter_dropped():
    q = to_quarterly(monthly(["2025-04", "2025-05", "2025-06", "2025-07", "2025-08"]))
    assert q.period.tolist() == ["2025Q2"]


def test_read_raw_missing_component_raises(tmp_path):
    p = tmp_path / "npci.json"
    p.write_text(json.dumps({"months": {
        "2025-Apr": resp([("JAMMU AND KASHMIR", None, "88", "1"), ("LADAKH", None, "3", "1")]),
        "2025-May": resp([("JAMMU AND KASHMIR", None, "90", "1")])}}))
    with pytest.raises(ValueError, match=r"jammu_kashmir.*2025-05.*ladakh"):
        read_raw(p)
