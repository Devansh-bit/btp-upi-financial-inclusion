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
