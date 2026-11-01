"""Extract NCP 2020 Table 8 (Persons, '000, 1 March) from pdftotext -layout output."""
import re
import subprocess
from pathlib import Path

import pandas as pd

PDF = Path("data/raw/population/ncp2020.pdf")
OUT = Path("data/manual/population_ncp2020.csv")
FIRST_PAGE, LAST_PAGE = 47, 59
YEARS = range(2018, 2028)
# Headers on these pages span several lines; the column order is fixed here.
EXPLICIT_NAMES = {
    "DAMAN & DIU": ["daman & diu", "dadra & nagar haveli", "goa"],
    "LAKSHADWEEP": ["lakshadweep", "puducherry", "andaman & nicobar islands"],
}
NOT_HEADER = ("TABLE", "Projected", "Territories", "('000)")


def _names(page: str) -> list[str]:
    for key, names in EXPLICIT_NAMES.items():
        if key in page:
            return names
    lines = page.splitlines()
    header = next(l for l in lines if re.match(r"^ {10,}[A-Z]", l) and not any(w in l for w in NOT_HEADER))
    return [re.sub(r"\*(\(UT\))?", "", n).strip().lower() for n in re.split(r" {3,}", header.strip())]


def parse_table8(text: str) -> pd.DataFrame:
    rows = []
    counts = []
    for page in (p for p in text.split("\f") if "TABLE" in p):
        names = _names(page)
        n = 0
        for line in page.splitlines():
            t = line.split()
            if t and re.fullmatch(r"20\d\d", t[0]) and int(t[0]) in YEARS:
                values = t[1:]
                if len(values) != 3 * len(names):
                    raise ValueError(f"unexpected column count in row: {line!r}")
                n += 1
                for j, name in enumerate(names):
                    rows.append((name, int(t[0]), int(values[3 * j].replace(",", ""))))
        counts.append(n)
    if len(set(counts)) != 1:
        raise ValueError(f"pages differ in number of year rows: {counts}")
    return pd.DataFrame(rows, columns=["raw_name", "year", "population_thousands"])


def main() -> None:
    text = subprocess.run(
        ["pdftotext", "-layout", "-f", str(FIRST_PAGE), "-l", str(LAST_PAGE), str(PDF), "-"],
        capture_output=True, text=True, check=True).stdout
    df = parse_table8(text)
    if len(df) != 38 * len(YEARS):
        raise ValueError(f"expected {38 * len(YEARS)} rows, got {len(df)}")
    df.to_csv(OUT, index=False)


if __name__ == "__main__":
    main()
