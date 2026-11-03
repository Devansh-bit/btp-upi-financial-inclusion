"""Parse PhonePe Pulse transaction counts by type (P2P, Retail, Utility), with data checks."""
from pathlib import Path

import pandas as pd

from btpdata.parse_phonepe import HOVER, SOURCE, _load
from btpdata.periods import quarters
from btpdata.states import sum_merged

TYPES = {"P2P": "phonepe_count_p2p", "Retail": "phonepe_count_retail", "Utility": "phonepe_count_utility"}
COLUMNS = ["state_id", "period", "variable", "value", "source"]
CHECK_COLUMNS = ["check", "n_items", "n_flagged", "max_abs_diff", "note"]
NATIONAL = "data/aggregated/transaction/country/india/{year}/{q}.json"
STATE_DIR = "data/aggregated/transaction/country/india/state"
USER_NATIONAL = "data/aggregated/user/country/india/{year}/{q}.json"


def folder_to_raw(folder: str) -> str:
    return folder.replace("-", " ")


def parse_type_file(path: Path) -> dict[str, int]:
    counts = {}
    for entry in _load(path)["transactionData"]:
        name = entry["name"]
        if name not in TYPES:
            raise ValueError(f"unknown transaction type {name!r} in {path}")
        (total,) = [m for m in entry["paymentInstruments"] if m["type"] == "TOTAL"]
        counts[name] = int(total["count"])
    return counts


def _hover_counts(path: Path) -> dict[str, int]:
    out = {}
    for entry in _load(path)["hoverDataList"]:
        (total,) = [m for m in entry["metric"] if m["type"] == "TOTAL"]
        out[entry["name"]] = int(total["count"])
    return out


def _hover_users(path: Path) -> int:
    return sum(int(v["registeredCount"]) for v in _load(path)["hoverData"].values())


def build_types(repo: Path, out: Path, checks_out: Path, periods: list[str]) -> tuple[pd.DataFrame, pd.DataFrame]:
    repo = Path(repo)
    folders = sorted(p.name for p in (repo / STATE_DIR).iterdir() if p.is_dir())
    rows, zeroed = [], []
    n_state_quarters = n_quarters = 0
    for period in periods:
        year, q = period.split("Q")
        hover = _hover_counts(repo / HOVER.format(kind="transaction", year=year, q=q))
        raw_of = {f: folder_to_raw(f) for f in folders}
        for name in sorted(set(hover) - set(raw_of.values())):
            raise ValueError(f"hover name {name!r} has no state folder in {period}")
        for f in folders:
            if raw_of[f] not in hover:
                raise ValueError(f"state folder {f!r} has no hover name in {period}")
        for f in folders:
            path = repo / STATE_DIR / f / year / f"{q}.json"
            if not path.exists():
                raise FileNotFoundError(f"missing type file: {path}")
            counts = parse_type_file(path)
            target = hover[raw_of[f]]
            absent = [t for t in TYPES if t not in counts]
            if sum(counts.values()) != target and not absent:
                raise ValueError(f"type counts sum to {sum(counts.values())}, hover count is {target}: {path}")
            if absent:
                if sum(counts.values()) != target:
                    raise ValueError(f"absent type {absent}; present types sum to {sum(counts.values())}, "
                                     f"hover count is {target}: {path}")
                zeroed += [f"{f} {period} {t}" for t in absent]
                counts.update({t: 0 for t in absent})
            n_state_quarters += 1
            rows += [(raw_of[f], period, TYPES[t], counts[t]) for t in TYPES]
        npath = repo / NATIONAL.format(year=year, q=q)
        national = sum(parse_type_file(npath).values())
        if national != sum(hover.values()):
            raise ValueError(f"national count {national} differs from state sum {sum(hover.values())}: {npath}")
        upath = repo / USER_NATIONAL.format(year=year, q=q)
        users = int(_load(upath)["aggregated"]["registeredCount"])
        state_users = _hover_users(repo / HOVER.format(kind="user", year=year, q=q))
        if users != state_users:
            raise ValueError(f"national users {users} differ from state sum {state_users}: {upath}")
        n_quarters += 1
    raw = pd.DataFrame(rows, columns=["raw_name", "period", "variable", "value"])
    df = sum_merged(raw, "raw_name", ["period", "variable"], ["value"])
    df["source"] = SOURCE
    df = df[COLUMNS]
    checks = pd.DataFrame(
        [
            ("type_sum_equals_state_total", n_state_quarters, 0, 0, ""),
            ("absent_type_set_to_zero", 3 * n_state_quarters, len(zeroed), 0, "; ".join(zeroed)),
            ("national_count_equals_state_sum", n_quarters, 0, 0, ""),
            ("national_users_equals_state_sum", n_quarters, 0, 0, ""),
        ],
        columns=CHECK_COLUMNS,
    )
    for p, d in ((out, df), (checks_out, checks)):
        Path(p).parent.mkdir(parents=True, exist_ok=True)
        d.to_csv(p, index=False)
    return df, checks


def main() -> None:
    build_types(Path("data/raw/phonepe/repo"), Path("data/interim/phonepe_types.csv"),
                Path("data/interim/phonepe_checks.csv"), quarters())


if __name__ == "__main__":
    main()
