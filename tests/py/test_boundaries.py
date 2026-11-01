import copy
import json
from pathlib import Path

import pytest

from btpdata.boundaries import add_state_ids
from btpdata.states import ALL_IDS, UnknownStateError

REAL = Path(__file__).resolve().parents[2] / "data/raw/boundaries/geoBoundaries-IND-ADM1_simplified.geojson"


def _fc(*names):
    return {"type": "FeatureCollection", "features": [
        {"type": "Feature", "properties": {"shapeName": n}, "geometry": None} for n in names]}


def test_adds_ids_without_mutating():
    src = _fc("Orissa", "Ladakh")
    before = copy.deepcopy(src)
    out = add_state_ids(src, "shapeName")
    assert [f["properties"]["state_id"] for f in out["features"]] == ["odisha", "jammu_kashmir"]
    assert src == before


def test_unknown_name_raises():
    with pytest.raises(UnknownStateError, match="Atlantis"):
        add_state_ids(_fc("Atlantis"), "shapeName")


def test_real_file():
    out = add_state_ids(json.loads(REAL.read_text(encoding="utf-8")), "shapeName")
    ids = [f["properties"]["state_id"] for f in out["features"]]
    assert len(ids) == 36
    assert set(ids) == set(ALL_IDS)
    assert ids.count("jammu_kashmir") == 2
