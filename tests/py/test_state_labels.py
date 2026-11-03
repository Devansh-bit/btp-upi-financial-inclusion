import csv
from pathlib import Path

from btpdata.states import ALL_IDS, MAIN_SAMPLE

LABELS = Path(__file__).resolve().parents[2] / "src" / "R" / "lib" / "state_labels.csv"


def _rows():
    with open(LABELS, newline="") as f:
        return list(csv.DictReader(f))


def test_ids_match_all_ids():
    ids = [r["state_id"] for r in _rows()]
    assert set(ids) == set(ALL_IDS)
    assert len(ids) == len(ALL_IDS)
    assert ids == sorted(ids)


def test_in_main_matches_main_sample():
    assert {r["state_id"] for r in _rows() if r["in_main"] == "TRUE"} == set(MAIN_SAMPLE)
    assert {r["in_main"] for r in _rows()} <= {"TRUE", "FALSE"}


def test_codes_two_upper_letters_unique():
    codes = [r["code"] for r in _rows()]
    assert all(len(c) == 2 and c.isalpha() and c.isupper() for c in codes)
    assert len(set(codes)) == len(codes)
