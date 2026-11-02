"""State-name crosswalk: maps raw state names from any source to a state_id."""
import csv
import re
import unicodedata
from pathlib import Path

import pandas as pd

_WHITESPACE = re.compile(r"\s+")


class UnknownStateError(ValueError):
    """Raised when a raw state name has no row in the crosswalk."""


def normalise(raw: str) -> str:
    decomposed = unicodedata.normalize("NFKD", raw)
    ascii_form = "".join(c for c in decomposed if not unicodedata.combining(c))
    return _WHITESPACE.sub(" ", ascii_form.lower().replace("&", "and").strip())


def _load_crosswalk() -> dict[str, str]:
    path = Path(__file__).with_name("state_crosswalk.csv")
    with path.open(newline="", encoding="utf-8") as f:
        return {row["raw_name"]: row["state_id"] for row in csv.DictReader(f)}


_CROSSWALK = _load_crosswalk()

_OTHER_UTS = ("andaman_nicobar", "chandigarh", "dnh_dd", "lakshadweep", "puducherry")

# Ids formed from more than one raw unit (Ladakh -> jammu_kashmir; DNH and Daman & Diu -> dnh_dd).
MERGED_IDS: tuple[str, ...] = ("jammu_kashmir", "dnh_dd")

ALL_IDS: tuple[str, ...] = tuple(sorted(set(_CROSSWALK.values())))
MAIN_SAMPLE: tuple[str, ...] = tuple(i for i in ALL_IDS if i not in _OTHER_UTS)


def to_state_id(raw: str) -> str:
    try:
        return _CROSSWALK[normalise(raw)]
    except KeyError:
        raise UnknownStateError(f"Unknown state name: {raw}") from None


def component(raw: str) -> str:
    """Component key of a raw name: normalised, without a leading "the ". Spellings of one unit
    give one key; a merged unit reported directly (e.g. "dadra & nagar haveli & daman & diu") is
    one component."""
    n = normalise(raw)
    return n[4:] if n.startswith("the ") else n


def sum_merged(df: pd.DataFrame, name_col: str, keys: list[str], value_cols: list[str]) -> pd.DataFrame:
    """Map df[name_col] to state_id and sum value_cols by state_id and keys.

    For each id in MERGED_IDS, the component set is the set of component keys seen for that id in
    the whole of df. Every key group of that id must contain the full set; a group with a missing
    component raises ValueError (id, key, missing component). A key with no row for the id is not
    checked. Output columns: state_id, *keys, *value_cols, sorted by state_id and keys."""
    d = df.assign(state_id=df[name_col].map(to_state_id), _component=df[name_col].map(component))
    for sid in MERGED_IDS:
        part = d[d.state_id == sid]
        expected = set(part._component)
        if len(expected) < 2:
            continue
        for key, g in part.groupby(keys):
            missing = expected - set(g._component)
            if missing:
                key = dict(zip(keys, key if isinstance(key, tuple) else (key,)))
                raise ValueError(f"{sid}: {key} lacks component(s) {sorted(missing)}; "
                                 f"the source gives {sorted(expected)}")
    return d.groupby(["state_id", *keys], as_index=False)[value_cols].sum()
