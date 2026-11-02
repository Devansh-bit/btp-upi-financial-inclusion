"""NPCI state-wise UPI statistics (volume in Mn, value in Cr) as monthly and quarterly state series.

Raw file: {"months": {"YYYY-Mon": API response}}. Only rows without a district (null, "" or "-") are used:
state rows (Format A, up to 2026-02) or "<STATE> Total" rows (Format B, from 2026-03).
"""
import json
from pathlib import Path

import pandas as pd

from .states import sum_merged

SOURCE = "npci_api"
RAW = Path("data/raw/npci/npci_statewise_2025-04_2026-09.json")
OUT = Path("data/interim/npci.csv")
UNATTRIBUTED_OUT = Path("data/final/npci_unattributed.csv")
UNATTRIBUTED_LABELS = frozenset({"unclassified"})
VOLUME_UNIT = 1e6  # Mn -> count
VALUE_UNIT = 1e7  # Cr -> INR
_MONTHS = {m: i for i, m in enumerate(
    ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"], 1)}
_TOTAL = " Total"
NO_DISTRICT = frozenset({"", "-"})


def _label_key(name: str) -> str:
    return name.replace("#", "").replace(" ", "").lower()


def _is_unattributed(name: str) -> bool:
    return _label_key(name) in UNATTRIBUTED_LABELS


def _number(text: str) -> float:
    return float(str(text).replace(",", ""))


def _no_district(value: str | None) -> bool:
    """Format B months mark state-level rows with null, "" or "-" in the district field."""
    return value is None or value.strip() in NO_DISTRICT


def _state_rows(response: dict) -> list[dict]:
    return [r for r in response["data"]["results"] if _no_district(r.get("district"))]


def _strip_total(name: str) -> str:
    name = name.strip()
    return name[: -len(_TOTAL)] if name.endswith(_TOTAL) else name


_VALUES = ["npci_volume", "npci_value_inr"]


def _month_rows(response: dict, month: str) -> pd.DataFrame:
    """One API response -> raw_name, month, npci_volume (count), npci_value_inr (rupees)."""
    rows = []
    for r in _state_rows(response):
        name = _strip_total(r["state_union_territory"])
        if _is_unattributed(name):
            continue
        rows.append({"raw_name": name, "month": month,
                     "npci_volume": _number(r["volume_in_mn"]) * VOLUME_UNIT,
                     "npci_value_inr": _number(r["value_in_cr"]) * VALUE_UNIT})
    return pd.DataFrame(rows, columns=["raw_name", "month", *_VALUES])


def parse_month(response: dict, month: str) -> pd.DataFrame:
    """One API response -> state_id, month, npci_volume (count), npci_value_inr (rupees)."""
    return sum_merged(_month_rows(response, month), "raw_name", ["month"], _VALUES)


def unattributed_share(response: dict) -> tuple[float, float]:
    """(volume share, value share) of the unclassified row in all district-null rows."""
    total = [0.0, 0.0]
    lost = [0.0, 0.0]
    for r in _state_rows(response):
        v, c = _number(r["volume_in_mn"]), _number(r["value_in_cr"])
        total[0] += v
        total[1] += c
        if _is_unattributed(_strip_total(r["state_union_territory"])):
            lost[0] += v
            lost[1] += c
    return lost[0] / total[0], lost[1] / total[1]


def _month_id(key: str) -> str:
    year, mon = key.split("-")
    return f"{year}-{_MONTHS[mon]:02d}"


def _load(path: Path) -> dict:
    return json.loads(Path(path).read_text(encoding="utf-8"))["months"]


def read_raw(path: Path) -> pd.DataFrame:
    """All months; merged-id components are inferred from the whole file (sum_merged)."""
    frames = [_month_rows(resp, _month_id(key)) for key, resp in _load(path).items()]
    return sum_merged(pd.concat(frames, ignore_index=True), "raw_name", ["month"], _VALUES)


def read_unattributed(path: Path) -> pd.DataFrame:
    rows = [(_month_id(k), *unattributed_share(resp)) for k, resp in _load(path).items()]
    return pd.DataFrame(rows, columns=["month", "volume_share", "value_share"])


def to_quarterly(df: pd.DataFrame) -> pd.DataFrame:
    """Sum the three months of each state-quarter; quarters with fewer than 3 months are dropped."""
    d = df.copy()
    year, month = d.month.str[:4], d.month.str[5:7].astype(int)
    d["period"] = year + "Q" + ((month - 1) // 3 + 1).astype(str)
    g = d.groupby(["state_id", "period"], as_index=False).agg(
        npci_volume=("npci_volume", "sum"), npci_value_inr=("npci_value_inr", "sum"),
        n_months=("month", "nunique"))
    g = g[g.n_months == 3].drop(columns="n_months")
    return g.reset_index(drop=True)


def main() -> None:
    df = read_raw(RAW)
    long = df.melt(id_vars=["state_id", "month"], value_vars=["npci_volume", "npci_value_inr"],
                   var_name="variable", value_name="value").rename(columns={"month": "period"})
    long["source"] = SOURCE
    long = long.sort_values(["variable", "state_id", "period"])
    OUT.parent.mkdir(parents=True, exist_ok=True)
    long[["state_id", "period", "variable", "value", "source"]].to_csv(OUT, index=False)
    un = read_unattributed(RAW)
    un.to_csv(UNATTRIBUTED_OUT, index=False)
    print(f"rows={len(long)} months={df.month.nunique()} "
          f"unattributed volume share {un.volume_share.min():.4f}-{un.volume_share.max():.4f}")


if __name__ == "__main__":
    main()
