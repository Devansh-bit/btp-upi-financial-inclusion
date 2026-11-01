import pandas as pd
import pytest

from btpdata.periods import quarters
from btpdata.population import load_annual, to_quarterly
from btpdata.states import ALL_IDS

REAL = "data/manual/population_ncp2020.csv"


def _raw(path):
    return pd.read_csv(path).set_index(["raw_name", "year"]).population_thousands


def test_interp():
    a = pd.DataFrame({"state_id": ["x"] * 2, "year": [2018, 2019], "population": [1000., 1400.]})
    q = to_quarterly(a, ["2018Q1", "2018Q2", "2018Q3", "2018Q4", "2019Q1"])
    assert q.population.tolist() == [1000., 1100., 1200., 1300., 1400.]


def test_merge(tmp_path):
    p = tmp_path / "pop.csv"
    p.write_text("raw_name,year,population_thousands\n"
                 "jammu & kashmir,2018,900\nladakh,2018,100\nindia,2018,5000\n")
    a = load_annual(p)
    assert a.query("state_id=='jammu_kashmir'").population.item() == 1_000_000
    assert list(a.columns) == ["state_id", "year", "population"]
    assert "india" not in a.state_id.tolist()


def test_needs_next_year():
    a = pd.DataFrame({"state_id": ["x"] * 9, "year": range(2018, 2027), "population": 1000.})
    with pytest.raises(ValueError, match="2027"):
        to_quarterly(a, ["2026Q2"])


def test_known_values():
    r = _raw(REAL)
    assert r["india", 2018] == 1318678
    assert r["jammu & kashmir", 2018] == 13101
    assert r["india", 2027] == 1433852
    assert r["himachal pradesh", 2026] == 7579


def test_real_file_sums():
    a = load_annual(REAL).groupby("year").population.sum()
    india = _raw(REAL).xs("india", level="raw_name") * 1000
    for y in range(2018, 2028):
        assert abs(a[y] / india[y] - 1) < 0.01


def test_real_file_complete():
    q = to_quarterly(load_annual(REAL), quarters())
    wide = q.pivot(index="period", columns="state_id", values="population")
    assert set(ALL_IDS) <= set(wide.columns)
    assert not wide[list(ALL_IDS)].isna().any().any()
    assert len(wide) == 34
