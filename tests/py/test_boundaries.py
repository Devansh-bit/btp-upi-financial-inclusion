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


def _manifest(dest, name, url, sha):
    dest.mkdir(parents=True, exist_ok=True)
    (dest / "manifest.csv").write_text(
        f"file,url,retrieved,sha256,notes\n{name},{url},2026-11-01,{sha},test\n", encoding="utf-8")


def test_fetch_downloads_and_verifies(tmp_path):
    import hashlib
    from btpdata.boundaries import FILENAME, fetch
    src = tmp_path / "src.geojson"
    src.write_bytes(b'{"type":"FeatureCollection","features":[]}')
    dest = tmp_path / "dest"
    _manifest(dest, FILENAME, src.as_uri(), hashlib.sha256(src.read_bytes()).hexdigest())
    path = fetch(dest)
    assert path == dest / FILENAME
    assert path.read_bytes() == src.read_bytes()


def test_fetch_sha_mismatch_raises(tmp_path):
    from btpdata.boundaries import FILENAME, fetch
    src = tmp_path / "src.geojson"
    src.write_bytes(b"{}")
    dest = tmp_path / "dest"
    _manifest(dest, FILENAME, src.as_uri(), "0" * 64)
    with pytest.raises(ValueError, match="sha256"):
        fetch(dest)


def test_fetch_present_matching_hash_no_download(tmp_path):
    import hashlib
    from btpdata.boundaries import FILENAME, fetch
    dest = tmp_path / "dest"
    body = b"{}"
    _manifest(dest, FILENAME, "file:///nonexistent", hashlib.sha256(body).hexdigest())
    (dest / FILENAME).write_bytes(body)
    assert fetch(dest) == dest / FILENAME


def test_fetch_present_wrong_hash_raises(tmp_path):
    import hashlib
    from btpdata.boundaries import FILENAME, fetch
    dest = tmp_path / "dest"
    _manifest(dest, FILENAME, "file:///nonexistent", "0" * 64)
    (dest / FILENAME).write_bytes(b"{}")
    actual = hashlib.sha256(b"{}").hexdigest()
    with pytest.raises(ValueError) as exc:
        fetch(dest)
    msg = str(exc.value)
    assert FILENAME in msg and "0" * 64 in msg and actual in msg and "delete" in msg
