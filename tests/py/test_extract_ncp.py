from pathlib import Path

from btpdata.extract_ncp import parse_table8

FIXTURE = Path(__file__).parent / "fixtures" / "ncp" / "page47_excerpt.txt"


def _df():
    return parse_table8(FIXTURE.read_text()).set_index(["raw_name", "year"]).population_thousands


def test_columns_and_names():
    df = parse_table8(FIXTURE.read_text())
    assert list(df.columns) == ["raw_name", "year", "population_thousands"]
    assert set(df.raw_name) == {"india", "jammu & kashmir", "himachal pradesh"}


def test_indian_grouping_and_persons_column():
    s = _df()
    assert s["india", 2018] == 1318678
    assert s["india", 2027] == 1433852
    assert s["jammu & kashmir", 2018] == 13101
    assert s["himachal pradesh", 2027] == 7606
