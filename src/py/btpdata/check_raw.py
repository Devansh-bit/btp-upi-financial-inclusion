"""Stop with a clear message if a manually obtained raw input is missing."""
import csv
import sys
from pathlib import Path

MANUAL = [
    Path("data/raw/population/ncp2020.pdf"),
    Path("data/raw/cpi/mospi_cpi_state_general_combined.json"),
    Path("data/raw/npci/npci_statewise_2025-04_2026-09.json"),
]


def url_for(path: Path) -> str:
    with (path.parent / "manifest.csv").open(encoding="utf-8", newline="") as fh:
        for row in csv.DictReader(fh):
            if row["file"] == path.name:
                return row["url"]
    return "(no manifest entry)"


def missing(paths=MANUAL) -> list[str]:
    return [f"MISSING raw input {p}\n  source URL: {url_for(p)}\n  see {p.parent}/manifest.csv"
            for p in paths if not p.is_file()]


def main() -> None:
    msgs = missing()
    if msgs:
        sys.exit("\n".join(msgs))
    print("raw inputs present")


if __name__ == "__main__":
    main()
