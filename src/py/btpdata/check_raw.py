"""Stop with a clear message if a manually obtained raw input is missing or its sha256 differs
from its manifest row."""
import csv
import hashlib
import sys
from pathlib import Path

MANUAL = [
    Path("data/raw/population/ncp2020.pdf"),
    Path("data/raw/cpi/mospi_cpi_state_general_combined.json"),
    Path("data/raw/npci/npci_statewise_2025-04_2026-09.json"),
]


def manifest_row(path: Path) -> dict | None:
    """Row of <dir>/manifest.csv whose `file` is path.name, or None."""
    with (path.parent / "manifest.csv").open(encoding="utf-8", newline="") as fh:
        for row in csv.DictReader(fh):
            if row["file"] == path.name:
                return row
    return None


def url_for(path: Path) -> str:
    row = manifest_row(path)
    return row["url"] if row else "(no manifest entry)"


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def missing(paths=MANUAL) -> list[str]:
    return [f"MISSING raw input {p}\n  source URL: {url_for(p)}\n  see {p.parent}/manifest.csv"
            for p in paths if not p.is_file()]


def hash_mismatches(paths=MANUAL) -> list[str]:
    """One message per present file whose sha256 differs from its manifest row, or whose
    manifest row has no sha256."""
    msgs = []
    for p in paths:
        if not p.is_file():
            continue
        row = manifest_row(p)
        expected = (row or {}).get("sha256", "").strip().lower()
        if not expected:
            msgs.append(f"NO SHA256 for raw input {p} in {p.parent}/manifest.csv")
            continue
        actual = sha256(p)
        if actual != expected:
            msgs.append(f"SHA256 MISMATCH for raw input {p}\n  expected (manifest): {expected}\n"
                        f"  actual:              {actual}")
    return msgs


def main(paths=None) -> None:
    paths = MANUAL if paths is None else paths
    msgs = missing(paths) + hash_mismatches(paths)
    if msgs:
        sys.exit("\n".join(msgs))
    print("raw inputs present; sha256 matches manifest")


if __name__ == "__main__":
    main()
