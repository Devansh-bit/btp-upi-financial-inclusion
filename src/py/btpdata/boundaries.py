"""Tag GeoJSON state features with state_id from the crosswalk."""
import copy
import csv
import hashlib
import json
import urllib.request
from pathlib import Path

from btpdata.states import to_state_id

FILENAME = "geoBoundaries-IND-ADM1_simplified.geojson"
RAW = Path("data/raw/boundaries") / FILENAME
OUT = Path("data/interim/india_states.geojson")


def add_state_ids(geojson: dict, name_field: str) -> dict:
    """Return a copy of geojson with properties.state_id on each feature."""
    out = copy.deepcopy(geojson)
    for feature in out["features"]:
        props = feature["properties"]
        props["state_id"] = to_state_id(props[name_field])
    return out


def fetch(dest: Path = Path("data/raw/boundaries")) -> Path:
    """Download the pinned GeoJSON if absent; verify sha256 from dest/manifest.csv."""
    target = dest / FILENAME
    with (dest / "manifest.csv").open(encoding="utf-8", newline="") as fh:
        row = next(r for r in csv.DictReader(fh) if r["file"] == FILENAME)
    if target.exists():
        body = target.read_bytes()
        digest = hashlib.sha256(body).hexdigest()
        if digest != row["sha256"]:
            raise ValueError(f"sha256 mismatch for existing {target}: got {digest}, manifest "
                             f"{row['sha256']}; delete the file to download it again")
        return target
    with urllib.request.urlopen(row["url"]) as resp:
        body = resp.read()
    digest = hashlib.sha256(body).hexdigest()
    if digest != row["sha256"]:
        raise ValueError(f"sha256 mismatch for {FILENAME}: got {digest}, manifest {row['sha256']}")
    dest.mkdir(parents=True, exist_ok=True)
    target.write_bytes(body)
    return target


def main() -> None:
    data = json.loads(RAW.read_text(encoding="utf-8"))
    tagged = add_state_ids(data, "shapeName")
    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(json.dumps(tagged, ensure_ascii=False), encoding="utf-8")
    print(f"wrote {OUT}: {len(tagged['features'])} features")


if __name__ == "__main__":
    main()
