import csv
import subprocess
from pathlib import Path

from btpdata.fetch_phonepe import SPARSE_PATHS, fetch


def make_repo(tmp_path: Path) -> tuple[Path, str]:
    src = tmp_path / "src"
    for path in SPARSE_PATHS:
        f = src / path / "2018" / "1.json"
        f.parent.mkdir(parents=True)
        f.write_text("{}")
    for args in (
        ["init", "-q"],
        ["add", "."],
        ["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "x"],
    ):
        subprocess.run(["git", "-C", str(src), *args], check=True)
    sha = subprocess.run(
        ["git", "-C", str(src), "rev-parse", "HEAD"], check=True, capture_output=True, text=True
    ).stdout.strip()
    return src, sha


def test_fetch_extends_sparse_set(tmp_path):
    src, sha = make_repo(tmp_path)
    old = SPARSE_PATHS[:2]
    repo = fetch(tmp_path / "raw", commit=sha, paths=old, url=str(src))
    assert (repo / old[0] / "2018/1.json").exists()
    assert not (repo / "data/aggregated/transaction/country/india/2018/1.json").exists()
    fetch(tmp_path / "raw", commit=sha, paths=SPARSE_PATHS, url=str(src))
    assert (repo / "data/aggregated/transaction/country/india/2018/1.json").exists()
    rows = list(csv.DictReader(open(tmp_path / "raw/manifest.csv")))
    assert len(rows) == 2 and "data/aggregated/user/country/india" in rows[1]["notes"]


def test_fetch_same_set_no_new_row(tmp_path):
    src, sha = make_repo(tmp_path)
    fetch(tmp_path / "raw", commit=sha, paths=SPARSE_PATHS, url=str(src))
    fetch(tmp_path / "raw", commit=sha, paths=SPARSE_PATHS, url=str(src))
    assert len(list(csv.DictReader(open(tmp_path / "raw/manifest.csv")))) == 1


def test_sparse_paths():
    assert len(SPARSE_PATHS) == 4
