"""State-name crosswalk: maps raw state names from any source to a state_id."""
import csv
import re
from pathlib import Path

_WHITESPACE = re.compile(r"\s+")


class UnknownStateError(ValueError):
    """Raised when a raw state name has no row in the crosswalk."""


def normalise(raw: str) -> str:
    return _WHITESPACE.sub(" ", raw.lower().replace("&", "and").strip())


def _load_crosswalk() -> dict[str, str]:
    path = Path(__file__).with_name("state_crosswalk.csv")
    with path.open(newline="", encoding="utf-8") as f:
        return {row["raw_name"]: row["state_id"] for row in csv.DictReader(f)}


_CROSSWALK = _load_crosswalk()

_OTHER_UTS = ("andaman_nicobar", "chandigarh", "dnh_dd", "lakshadweep", "puducherry")

ALL_IDS: tuple[str, ...] = tuple(sorted(set(_CROSSWALK.values())))
MAIN_SAMPLE: tuple[str, ...] = tuple(i for i in ALL_IDS if i not in _OTHER_UTS)


def to_state_id(raw: str) -> str:
    try:
        return _CROSSWALK[normalise(raw)]
    except KeyError:
        raise UnknownStateError(f"Unknown state name: {raw}") from None
