"""NCP 2020 projected population (1 March), annual to quarterly, in the interim long schema."""
from pathlib import Path

import pandas as pd

from .periods import quarters
from .states import to_state_id

SOURCE = "ncp2020_table8"


def load_annual(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    df = df[df.raw_name.str.strip().str.lower() != "india"].copy()
    df["state_id"] = df.raw_name.map(to_state_id)
    df["population"] = df.population_thousands * 1000.0
    return (df.groupby(["state_id", "year"], as_index=False).population.sum()
              .sort_values(["state_id", "year"]).reset_index(drop=True))


def to_quarterly(annual: pd.DataFrame, periods: list[str]) -> pd.DataFrame:
    table = annual.set_index(["state_id", "year"]).population
    rows = []
    for state_id in annual.state_id.unique():
        for period in periods:
            year, q = int(period[:4]), int(period[5])
            base = table.get((state_id, year))
            if base is None:
                raise ValueError(f"{state_id}: population for year {year} missing")
            if q == 1:
                value = base
            else:
                nxt = table.get((state_id, year + 1))
                if nxt is None:
                    raise ValueError(f"{state_id}: population for year {year + 1} missing (needed for {period})")
                value = base + (q - 1) / 4 * (nxt - base)
            rows.append((state_id, period, float(value)))
    return pd.DataFrame(rows, columns=["state_id", "period", "population"])


def main() -> None:
    q = to_quarterly(load_annual(Path("data/manual/population_ncp2020.csv")), quarters())
    out = q.rename(columns={"population": "value"})
    out["variable"] = "population"
    out["source"] = SOURCE
    out = out[["state_id", "period", "variable", "value", "source"]]
    Path("data/interim").mkdir(parents=True, exist_ok=True)
    out.to_csv("data/interim/population.csv", index=False)


if __name__ == "__main__":
    main()
