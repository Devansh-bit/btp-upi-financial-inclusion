"""State CPI (Combined, General) on base 2012=100, quarterly, in the interim long schema.

Base 2012 (Jan 2017 - Dec 2025) is linked to base 2024 (Jan 2025 onward) with a per-state
overlap factor f_s = mean(base 2012, 2025) / mean(base 2024, 2025). Base 2024 values enter
from 2026-01 as index * f_s.
"""
import json
from pathlib import Path

import numpy as np
import pandas as pd

from .periods import quarters
from .states import ALL_IDS, normalise, to_state_id

SOURCE = "mospi_cpi_2012linked"
ALL_INDIA = "all_india"
OVERLAP_YEAR = "2025"
LINK_MONTH = "2026-01"  # first month taken from base 2024
MAX_GAP = 2
RAW = Path("data/raw/cpi/mospi_cpi_state_general_combined.json")
POPULATION = Path("data/manual/population_ncp2020.csv")
OUT = Path("data/interim/cpi.csv")
_MONTHS = {m: i for i, m in enumerate(
    ["January", "February", "March", "April", "May", "June", "July", "August",
     "September", "October", "November", "December"], 1)}
_DNH, _DAMAN = "dadra and nagar haveli", "daman and diu"


def _larger_dnh_dd_component(path: Path = POPULATION) -> str:
    """Normalised name of the larger of the two components by NCP 2018 population."""
    pop = pd.read_csv(path)
    pop = pop[pop.year == 2018].assign(name=lambda d: d.raw_name.map(normalise))
    pop = pop[pop.name.isin([_DNH, _DAMAN])].set_index("name").population_thousands
    return pop.idxmax()


def _state_key(name: str, base: str, dnh_dd_component: str) -> str | None:
    """State id for a raw MoSPI name, or None for a row that must be dropped."""
    n = normalise(name)
    if n == "all india":
        return ALL_INDIA
    if n == "ladakh":  # jammu_kashmir uses the J&K series
        return None
    if base == "2012" and n in (_DNH, _DAMAN) and n != dnh_dd_component:
        return None
    return to_state_id(name)


def read_raw(path: Path) -> pd.DataFrame:
    data = json.loads(Path(path).read_text(encoding="utf-8"))["data"]
    component = _larger_dnh_dd_component()
    frames = []
    for base, key in (("2012", "base2012"), ("2024", "base2024")):
        rows = pd.DataFrame(data[key]["rows"])
        ids = rows.state.map(lambda n: _state_key(n, base, component))
        month = (rows.year.astype(int).astype(str) + "-"
                 + rows.month.map(_MONTHS).map("{:02d}".format))
        frames.append(pd.DataFrame({
            "state_id": ids, "month": month,
            "cpi": pd.to_numeric(rows["index"], errors="coerce").astype(float),
            "base": base})[ids.notna()])
    df = pd.concat(frames, ignore_index=True).dropna(subset=["cpi"])
    return df.sort_values(["state_id", "base", "month"]).reset_index(drop=True)


def overlap_factors(df: pd.DataFrame) -> dict[str, float]:
    """Per-state f_s = mean(base 2012) / mean(base 2024) over the months of OVERLAP_YEAR
    present in both bases."""
    ov = df[df.month.str.startswith(OVERLAP_YEAR)]
    wide = ov.pivot_table(index=["state_id", "month"], columns="base", values="cpi").dropna()
    out = {}
    for sid, g in wide.groupby(level="state_id"):
        out[sid] = float(g["2012"].mean() / g["2024"].mean())
    return out


def link_bases(df: pd.DataFrame, factors: dict[str, float]) -> pd.DataFrame:
    """All values on base 2012=100: base 2012 before LINK_MONTH, base 2024 * factor from it."""
    old = df[(df.base == "2012") & (df.month < LINK_MONTH)]
    new = df[(df.base == "2024") & (df.month >= LINK_MONTH)].copy()
    new["cpi"] = new.cpi * new.state_id.map(factors)
    out = pd.concat([old, new], ignore_index=True)
    return out.sort_values(["state_id", "month"]).reset_index(drop=True)


def fill_gaps(df: pd.DataFrame) -> pd.DataFrame:
    """Fill interior gaps of at most MAX_GAP months by linear interpolation of log(cpi).
    Adds boolean column `interpolated`."""
    parts = []
    for sid, g in df.groupby("state_id"):
        s = g.set_index(pd.PeriodIndex(g.month, freq="M")).cpi.sort_index()
        full = s.reindex(pd.period_range(s.index.min(), s.index.max(), freq="M"))
        missing = full.isna()
        run_id = (missing != missing.shift()).cumsum()
        run_len = missing.groupby(run_id).transform("sum")
        fillable = missing & (run_len <= MAX_GAP)
        logs = np.log(full).interpolate(method="linear", limit_area="inside")
        filled = full.where(~fillable, np.exp(logs))
        keep = ~missing | fillable
        parts.append(pd.DataFrame({
            "state_id": sid, "month": filled.index.strftime("%Y-%m")[keep.values],
            "cpi": filled[keep].values, "interpolated": fillable[keep].values}))
    if not parts:
        return df.assign(interpolated=False)
    return pd.concat(parts, ignore_index=True)


def to_quarterly(df: pd.DataFrame) -> pd.DataFrame:
    """Quarterly mean; NaN unless all 3 months are present. Adds cpi_interpolated_months
    when the input has an `interpolated` column."""
    d = df.copy()
    d["period"] = d.month.str[:4] + "Q" + ((d.month.str[5:7].astype(int) - 1) // 3 + 1).astype(str)
    agg = {"cpi": ("cpi", "mean"), "n": ("cpi", "count")}
    if "interpolated" in d:
        agg["cpi_interpolated_months"] = ("interpolated", "sum")
    out = d.groupby(["state_id", "period"], as_index=False).agg(**agg)
    out.loc[out.n < 3, "cpi"] = np.nan
    out = out.drop(columns="n")
    if "cpi_interpolated_months" in out:
        out["cpi_interpolated_months"] = out.cpi_interpolated_months.astype(int)
    return out


def apply_fallback(qdf: pd.DataFrame, ids, periods: list[str] | None = None) -> pd.DataFrame:
    """One row per id x period. An id with no own series gets the all-India series and
    cpi_fallback = 1."""
    periods = periods or sorted(qdf.period.unique())
    own = set(qdf.state_id)
    cols = [c for c in qdf.columns if c not in ("state_id", "period")]
    frames = []
    for sid in ids:
        src = sid if sid in own else ALL_INDIA
        part = (qdf[qdf.state_id == src].set_index("period")
                .reindex(periods)[cols].reset_index().rename(columns={"index": "period"}))
        part.insert(0, "state_id", sid)
        part["cpi_fallback"] = 0 if sid in own else 1
        frames.append(part)
    return pd.concat(frames, ignore_index=True)


def build(df: pd.DataFrame, ids=ALL_IDS, periods: list[str] | None = None):
    factors = overlap_factors(df)
    quarterly = to_quarterly(fill_gaps(link_bases(df, factors)))
    return apply_fallback(quarterly, ids, periods or quarters()), factors


def main() -> None:
    df = read_raw(RAW)
    wide, factors = build(df)
    if "cpi_interpolated_months" in wide:
        wide["cpi_interpolated_months"] = wide.cpi_interpolated_months.fillna(0).astype(int)
    long = wide.melt(id_vars=["state_id", "period"],
                     value_vars=["cpi", "cpi_fallback", "cpi_interpolated_months"],
                     var_name="variable", value_name="value")
    long["source"] = SOURCE
    long = long.sort_values(["variable", "state_id", "period"])
    OUT.parent.mkdir(parents=True, exist_ok=True)
    long[["state_id", "period", "variable", "value", "source"]].to_csv(OUT, index=False)
    f = pd.Series(factors)
    print(f"rows={len(long)} nan_cpi={int(wide.cpi.isna().sum())} "
          f"factors min={f.min():.4f} median={f.median():.4f} max={f.max():.4f}")


if __name__ == "__main__":
    main()
