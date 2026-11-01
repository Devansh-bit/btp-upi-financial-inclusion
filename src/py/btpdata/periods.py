"""Quarter period strings of the form YYYYQn."""


def _split(period: str) -> tuple[int, int]:
    year, quarter = period.split("Q")
    return int(year), int(quarter)


def quarters(start: str = "2018Q1", end: str = "2026Q2") -> list[str]:
    year, q = _split(start)
    end_year, end_q = _split(end)
    result = []
    while (year, q) <= (end_year, end_q):
        result.append(f"{year}Q{q}")
        year, q = (year, q + 1) if q < 4 else (year + 1, 1)
    return result
