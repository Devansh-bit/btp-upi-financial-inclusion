"""Build the quarterly analysis panel from the interim files and check it."""
from itertools import product
from pathlib import Path

import numpy as np
import pandas as pd

from btpdata.periods import quarters
from btpdata.states import ALL_IDS, MAIN_SAMPLE

INTERIM_FILES = ("phonepe.csv", "population.csv", "cpi.csv")
KEYS = ["state_id", "period"]
BASE_COLUMNS = ["phonepe_count", "phonepe_amount_inr", "phonepe_users", "population", "cpi", "cpi_fallback",
                "cpi_interpolated_months"]
PER_CAPITA = ["count_pc", "real_amount_pc", "users_per_1000"]
LOG_COLUMNS = {"ln_count_pc": "count_pc", "ln_real_amount_pc": "real_amount_pc"}
COLUMNS = KEYS + ["in_main_sample"] + BASE_COLUMNS + PER_CAPITA + list(LOG_COLUMNS)
OUTLIER_FACTOR = 10
NEGATIVE_CHECKED = BASE_COLUMNS[:5] + PER_CAPITA

FINAL_DIR = Path("data/final")
INTERIM_DIR = Path("data/interim")


class PanelCheckError(ValueError):
    """Raised when the panel fails a structural or value check."""


def _read_wide(path: Path) -> pd.DataFrame:
    if not path.exists():
        raise FileNotFoundError(path)
    long = pd.read_csv(path)
    return long.pivot(index=KEYS, columns="variable", values="value").reset_index()


def _safe_ratio(numerator: pd.Series, denominator: pd.Series) -> pd.Series:
    """Ratio that is NaN where the denominator is missing or not positive."""
    return numerator / denominator.where(denominator > 0)


def _safe_ln(values: pd.Series) -> pd.Series:
    positive = values > 0
    out = np.full(len(values), np.nan)
    out[positive.to_numpy()] = np.log(values[positive].to_numpy())
    return pd.Series(out, index=values.index)


def build_quarterly(interim_dir: Path) -> pd.DataFrame:
    interim_dir = Path(interim_dir)
    wide = [_read_wide(interim_dir / name) for name in INTERIM_FILES]
    df = wide[0]
    for other in wide[1:]:
        df = df.merge(other, on=KEYS, how="outer")
    periods = sorted(df["period"].unique())
    expected = set(product(ALL_IDS, periods))
    got = list(zip(df.state_id, df.period))
    if len(got) != len(expected) or set(got) != expected:
        raise ValueError(
            f"merge key set differs from expected: {len(got)} rows, {len(expected)} expected, "
            f"unexpected={sorted(set(got) - expected)[:5]}, missing={sorted(expected - set(got))[:5]}")
    for column in BASE_COLUMNS:
        if column not in df.columns:
            raise ValueError(f"variable missing from interim files: {column}")
    df["in_main_sample"] = df.state_id.isin(MAIN_SAMPLE)
    df["count_pc"] = _safe_ratio(df.phonepe_count, df.population)
    df["real_amount_pc"] = _safe_ratio(df.phonepe_amount_inr * 100 / df.cpi.where(df.cpi > 0), df.population)
    df["users_per_1000"] = _safe_ratio(df.phonepe_users * 1000, df.population)
    for ln_name, source in LOG_COLUMNS.items():
        df[ln_name] = _safe_ln(df[source])
    return df[COLUMNS].sort_values(KEYS).reset_index(drop=True)


def check_panel(df: pd.DataFrame) -> None:
    dup = df.duplicated(KEYS, keep=False)
    if dup.any():
        first = df.loc[dup, KEYS].iloc[0]
        raise PanelCheckError(f"duplicate key rows: {int(dup.sum())}, first {first.state_id} {first.period}")
    for column in NEGATIVE_CHECKED:
        negative = df[column] < 0
        if negative.any():
            row = df.loc[negative].iloc[0]
            raise PanelCheckError(f"negative value {row.state_id} {row.period} {column}={row[column]}")
    for column in PER_CAPITA:
        median = df.groupby("period")[column].transform("median")
        bad = df[column] > OUTLIER_FACTOR * median
        if bad.any():
            row = df.loc[bad].iloc[0]
            raise PanelCheckError(f"outlier {row.state_id} {row.period} {column}={row[column]}")


def outliers(df: pd.DataFrame) -> pd.DataFrame:
    """All rows that violate the outlier rule, with the period median."""
    frames = []
    for column in PER_CAPITA:
        median = df.groupby("period")[column].transform("median")
        bad = df[column] > OUTLIER_FACTOR * median
        frames.append(pd.DataFrame({"state_id": df.state_id[bad], "period": df.period[bad], "column": column,
                                    "value": df[column][bad], "period_median": median[bad]}))
    return pd.concat(frames, ignore_index=True)


def coverage(df: pd.DataFrame) -> pd.DataFrame:
    variables = [c for c in df.columns if c not in KEYS]
    missing = df[variables].isna().groupby(df.state_id).sum()
    long = missing.stack().rename("n_missing").reset_index()
    long.columns = ["state_id", "variable", "n_missing"]
    long["n_missing"] = long["n_missing"].astype(int)
    return long[["variable", "state_id", "n_missing"]].sort_values(["variable", "state_id"]).reset_index(drop=True)


def main() -> None:
    df = build_quarterly(INTERIM_DIR)
    if sorted(df.period.unique()) != quarters():
        raise ValueError("panel periods differ from 2018Q1-2026Q2")
    try:
        check_panel(df)
    except PanelCheckError:
        print(outliers(df).to_string())
        raise
    FINAL_DIR.mkdir(parents=True, exist_ok=True)
    df.to_csv(FINAL_DIR / "panel_quarterly.csv", index=False)
    coverage(df).to_csv(FINAL_DIR / "coverage_report.csv", index=False)
    print(f"rows={len(df)}")
    print(df[PER_CAPITA + list(LOG_COLUMNS)].isna().sum().to_string())


if __name__ == "__main__":
    main()
