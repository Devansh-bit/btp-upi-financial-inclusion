"""Tag GeoJSON state features with state_id from the crosswalk."""
import copy
import json
from pathlib import Path

from btpdata.states import to_state_id

RAW = Path("data/raw/boundaries/geoBoundaries-IND-ADM1_simplified.geojson")
OUT = Path("data/interim/india_states.geojson")


def add_state_ids(geojson: dict, name_field: str) -> dict:
    """Return a copy of geojson with properties.state_id on each feature."""
    out = copy.deepcopy(geojson)
    for feature in out["features"]:
        props = feature["properties"]
        props["state_id"] = to_state_id(props[name_field])
    return out


def main() -> None:
    data = json.loads(RAW.read_text(encoding="utf-8"))
    tagged = add_state_ids(data, "shapeName")
    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(json.dumps(tagged, ensure_ascii=False), encoding="utf-8")
    print(f"wrote {OUT}: {len(tagged['features'])} features")


if __name__ == "__main__":
    main()
