"""Check that PhonePe Pulse tracks official NPCI state-wise UPI data where both exist."""
from pathlib import Path

import pandas as pd

from .npci import RAW, read_raw, to_quarterly

PANEL = Path("data/final/panel_quarterly.csv")
OUT = Path("data/final/validation_npci.csv")
COLUMNS = ["period", "measure", "pearson", "spearman", "n_states"]


def shares(x: pd.Series) -> pd.Series:
    return x / x.sum()


def spearman(a: pd.Series, b: pd.Series) -> float:
    """Spearman correlation as Pearson on average ranks (pandas method="spearman" needs scipy)."""
    return a.rank().corr(b.rank(), method="pearson")


def _measures(m: pd.DataFrame) -> dict[str, tuple[pd.Series, pd.Series]]:
    return {
        "share_count": (shares(m.phonepe_count), shares(m.npci_volume)),
        "share_value": (shares(m.phonepe_amount_inr), shares(m.npci_value_inr)),
        "count_pc": (m.phonepe_count / m.population, m.npci_volume / m.population),
        "value_pc": (m.phonepe_amount_inr / m.population, m.npci_value_inr / m.population),
    }


def compare(panel: pd.DataFrame, npci_q: pd.DataFrame) -> pd.DataFrame:
    """Per period, correlate PhonePe and NPCI over the states present in both."""
    merged = panel.merge(npci_q, on=["state_id", "period"], how="inner")
    rows = []
    for period, m in merged.groupby("period", sort=True):
        for name, (phonepe, npci) in _measures(m).items():
            rows.append({"period": period, "measure": name,
                         "pearson": phonepe.corr(npci, method="pearson"),
                         "spearman": spearman(phonepe, npci),
                         "n_states": len(m)})
    return pd.DataFrame(rows, columns=COLUMNS)


def main() -> None:
    panel = pd.read_csv(PANEL)
    result = compare(panel, to_quarterly(read_raw(RAW)))
    OUT.parent.mkdir(parents=True, exist_ok=True)
    result.to_csv(OUT, index=False)
    print(result.to_string(index=False))


if __name__ == "__main__":
    main()
