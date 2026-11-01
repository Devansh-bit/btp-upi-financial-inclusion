"""Parse PhonePe Pulse hover JSON files into the interim long table."""
import json
from pathlib import Path

import pandas as pd

from btpdata.fetch_phonepe import PHONEPE_COMMIT
from btpdata.periods import quarters
from btpdata.states import to_state_id

COLUMNS = ["state_id", "period", "variable", "value", "source"]
SOURCE = f"phonepe@{PHONEPE_COMMIT[:7]}"
HOVER = "data/map/{kind}/hover/country/india/{year}/{q}.json"


def _rows(items, period: str, variable: str) -> pd.DataFrame:
    return pd.DataFrame(
        [(to_state_id(name), period, variable, value) for name, value in items],
        columns=COLUMNS[:4],
    )


def _load(path: Path) -> dict:
    with Path(path).open(encoding="utf-8") as f:
        return json.load(f)["data"]


def parse_transactions(path: Path, period: str) -> pd.DataFrame:
    totals = []
    for entry in _load(path)["hoverDataList"]:
        (total,) = [m for m in entry["metric"] if m["type"] == "TOTAL"]
        totals.append((entry["name"], total))
    return pd.concat(
        [
            _rows(((n, int(t["count"])) for n, t in totals), period, "phonepe_count"),
            _rows(((n, float(t["amount"])) for n, t in totals), period, "phonepe_amount_inr"),
        ],
        ignore_index=True,
    )


def parse_users(path: Path, period: str) -> pd.DataFrame:
    hover = _load(path)["hoverData"]
    return _rows(
        ((n, int(v["registeredCount"])) for n, v in hover.items()), period, "phonepe_users"
    )


def build_interim(repo: Path, out: Path, periods: list[str]) -> pd.DataFrame:
    frames = []
    for period in periods:
        year, q = period.split("Q")
        paths = {k: Path(repo) / HOVER.format(kind=k, year=year, q=q) for k in ("transaction", "user")}
        frames.append(parse_transactions(paths["transaction"], period))
        frames.append(parse_users(paths["user"], period))
    df = (
        pd.concat(frames, ignore_index=True)
        .groupby(["state_id", "period", "variable"], as_index=False)["value"]
        .sum()
    )
    df["source"] = SOURCE
    df = df[COLUMNS]
    Path(out).parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(out, index=False)
    return df


def main() -> None:
    build_interim(Path("data/raw/phonepe/repo"), Path("data/interim/phonepe.csv"), quarters())


if __name__ == "__main__":
    main()
