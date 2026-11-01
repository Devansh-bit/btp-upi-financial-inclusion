from pathlib import Path

import pytest

from btpdata.parse_phonepe import build_interim
from btpdata.periods import quarters

FIX = Path(__file__).parent / "fixtures" / "phonepe"


def test_quarters():
    q = quarters()
    assert len(q) == 34 and q[0] == "2018Q1" and q[-1] == "2026Q2"


def test_parse_and_merge(tmp_path):
    df = build_interim(FIX, tmp_path / "p.csv", ["2018Q1"])
    assert list(df.columns) == ["state_id", "period", "variable", "value", "source"]
    v = df.set_index(["state_id", "variable"])["value"]
    assert v[("andhra_pradesh", "phonepe_count")] == 10862829
    assert v[("jammu_kashmir", "phonepe_count")] == 110
    assert v[("jammu_kashmir", "phonepe_amount_inr")] == 1050.0
    assert v[("jammu_kashmir", "phonepe_users")] == 45
    assert "ladakh" not in set(df.state_id)


def test_missing_quarter_fails(tmp_path):
    with pytest.raises(FileNotFoundError, match="2018/2.json"):
        build_interim(FIX, tmp_path / "p.csv", ["2018Q1", "2018Q2"])
