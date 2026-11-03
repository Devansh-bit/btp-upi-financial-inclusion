"""Clone PhonePe Pulse at a pinned commit (sparse checkout) and record a manifest row."""
import csv
import subprocess
from datetime import date
from pathlib import Path

PHONEPE_COMMIT = "257bf38e3095020f9f516c4c72c10bdc348781b1"
PHONEPE_URL = "https://github.com/PhonePe/pulse.git"
SPARSE_PATHS = [
    "data/map/transaction/hover/country/india",
    "data/map/user/hover/country/india",
    "data/aggregated/transaction/country/india",
    "data/aggregated/user/country/india",
]
MANIFEST_FIELDS = ["file", "url", "retrieved", "sha256", "notes"]


def _git(repo: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", "-C", str(repo), *args], check=True, capture_output=True, text=True
    )
    return result.stdout.strip()


def _at_commit(repo: Path, commit: str) -> bool:
    if not repo.exists():
        return False
    try:
        return _git(repo, "rev-parse", "HEAD") == commit
    except subprocess.CalledProcessError:
        return False


def _append_manifest(dest: Path, commit: str, paths: list[str], url: str) -> None:
    manifest = dest / "manifest.csv"
    is_new = not manifest.exists()
    with manifest.open("a", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=MANIFEST_FIELDS)
        if is_new:
            writer.writeheader()
        writer.writerow(
            {
                "file": "repo",
                "url": url,
                "retrieved": date.today().isoformat(),
                "sha256": "",
                "notes": f"git commit {commit}; sparse paths: {';'.join(paths)}",
            }
        )


def fetch(
    dest: Path,
    commit: str = PHONEPE_COMMIT,
    paths: list[str] | None = None,
    url: str = PHONEPE_URL,
) -> Path:
    paths = SPARSE_PATHS if paths is None else paths
    dest = Path(dest)
    repo = dest / "repo"
    if _at_commit(repo, commit):
        current = _git(repo, "sparse-checkout", "list").splitlines()
        if set(current) == set(paths):
            return repo
    dest.mkdir(parents=True, exist_ok=True)
    if not repo.exists():
        subprocess.run(
            ["git", "clone", "--filter=blob:none", "--no-checkout", url, str(repo)],
            check=True,
        )
    _git(repo, "sparse-checkout", "set", *paths)
    _git(repo, "checkout", commit)
    _append_manifest(dest, commit, paths, url)
    return repo


def main() -> None:
    fetch(Path("data/raw/phonepe"))


if __name__ == "__main__":
    main()
