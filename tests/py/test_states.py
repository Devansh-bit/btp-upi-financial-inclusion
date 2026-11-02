import pytest

from btpdata.states import ALL_IDS, MAIN_SAMPLE, UnknownStateError, normalise, to_state_id

PHONEPE_36 = [
    "andaman & nicobar islands", "andhra pradesh", "arunachal pradesh", "assam", "bihar",
    "chandigarh", "chhattisgarh", "dadra & nagar haveli & daman & diu", "delhi", "goa",
    "gujarat", "haryana", "himachal pradesh", "jammu & kashmir", "jharkhand", "karnataka",
    "kerala", "ladakh", "lakshadweep", "madhya pradesh", "maharashtra", "manipur",
    "meghalaya", "mizoram", "nagaland", "odisha", "puducherry", "punjab", "rajasthan",
    "sikkim", "tamil nadu", "telangana", "tripura", "uttar pradesh", "uttarakhand",
    "west bengal",
]


@pytest.mark.parametrize("raw,sid", [
    ("Orissa", "odisha"), ("ODISHA ", "odisha"),
    ("Andaman And Nicobar Islands", "andaman_nicobar"),
    ("andaman & nicobar islands", "andaman_nicobar"),
    ("Jammu and Kashmir", "jammu_kashmir"), ("ladakh", "jammu_kashmir"),
    ("NCT of Delhi", "delhi"), ("Pondicherry", "puducherry"),
    ("dadra & nagar haveli & daman & diu", "dnh_dd"),
    ("Dadra and Nagar Haveli", "dnh_dd"), ("Daman and Diu", "dnh_dd"),
    ("Uttaranchal", "uttarakhand"),
    ("Bihār", "bihar"), ("Dādra and Nagar Haveli and Damān and Diu", "dnh_dd"),
    ("Jammu and Kashmīr", "jammu_kashmir"),
])
def test_variants(raw, sid):
    assert to_state_id(raw) == sid


def test_normalise():
    assert normalise("  Andaman &  Nicobar\tIslands ") == "andaman and nicobar islands"


def test_unknown():
    with pytest.raises(UnknownStateError, match="Atlantis"):
        to_state_id("Atlantis")


def test_phonepe_names():
    assert {to_state_id(n) for n in PHONEPE_36} == set(ALL_IDS)


def test_samples():
    assert len(MAIN_SAMPLE) == 30 and len(ALL_IDS) == 35
    assert set(MAIN_SAMPLE) <= set(ALL_IDS)


def test_normalise_strips_diacritics():
    assert normalise("Chhattīsgarh") == "chhattisgarh"


def test_cpi_merged_unit_name():
    assert to_state_id("The Dadra And Nagar Haveli And Daman And Diu") == "dnh_dd"


def _merged_input(rows):
    import pandas as pd
    return pd.DataFrame(rows, columns=["raw_name", "period", "value"])


def test_sum_merged_jammu_kashmir_ladakh():
    from btpdata.states import sum_merged
    df = _merged_input([("jammu & kashmir", "2018Q1", 100), ("ladakh", "2018Q1", 10),
                        ("bihar", "2018Q1", 5)])
    out = sum_merged(df, "raw_name", ["period"], ["value"])
    assert list(out.columns) == ["state_id", "period", "value"]
    assert out.set_index("state_id").value.to_dict() == {"bihar": 5, "jammu_kashmir": 110}


def test_sum_merged_dnh_dd_components():
    from btpdata.states import sum_merged
    df = _merged_input([("Dadra and Nagar Haveli", "2018Q1", 1.0), ("Daman and Diu", "2018Q1", 2.0),
                        ("Dadra and Nagar Haveli", "2018Q2", 3.0), ("DAMAN & DIU", "2018Q2", 4.0)])
    out = sum_merged(df, "raw_name", ["period"], ["value"])
    assert out.value.tolist() == [3.0, 7.0] and set(out.state_id) == {"dnh_dd"}


def test_sum_merged_missing_component_raises():
    from btpdata.states import sum_merged
    df = _merged_input([("jammu & kashmir", "2018Q1", 100), ("ladakh", "2018Q1", 10),
                        ("jammu & kashmir", "2018Q2", 100)])
    with pytest.raises(ValueError, match=r"jammu_kashmir.*2018Q2.*ladakh"):
        sum_merged(df, "raw_name", ["period"], ["value"])


def test_sum_merged_direct_merged_unit_is_one_component():
    from btpdata.states import sum_merged
    df = _merged_input([("dadra & nagar haveli & daman & diu", "2018Q1", 7),
                        ("The Dadra and Nagar Haveli and Daman and Diu", "2018Q2", 8),
                        ("jammu & kashmir", "2018Q1", 1), ("jammu & kashmir", "2018Q2", 2)])
    out = sum_merged(df, "raw_name", ["period"], ["value"])
    assert out.set_index(["state_id", "period"]).value.to_dict() == {
        ("dnh_dd", "2018Q1"): 7, ("dnh_dd", "2018Q2"): 8,
        ("jammu_kashmir", "2018Q1"): 1, ("jammu_kashmir", "2018Q2"): 2}
