import json
import shutil
from pathlib import Path

import pandas as pd
import pytest

from btpdata.parse_phonepe_types import build_types, folder_to_raw
from btpdata.states import to_state_id

FIX = Path(__file__).parent / "fixtures" / "phonepe_types"
REAL = Path("data/raw/phonepe/repo/data/aggregated/transaction/country/india/state")
AGG = "data/aggregated/transaction/country/india"


def _copy(tmp_path):
    dst = tmp_path / "repo"
    shutil.copytree(FIX, dst)
    return dst


def _edit(path, fn):
    d = json.loads(path.read_text())
    fn(d["data"]["transactionData"])
    path.write_text(json.dumps(d))


def _run(repo, tmp_path):
    return build_types(repo, tmp_path / "t.csv", tmp_path / "c.csv", ["2018Q1"])


def test_folder_to_raw():
    assert to_state_id(folder_to_raw("jammu-&-kashmir")) == "jammu_kashmir"


def test_values(tmp_path):
    df, chk = build_types(FIX, tmp_path / "t.csv", tmp_path / "c.csv", ["2018Q1"])
    v = df.set_index(["state_id", "variable"]).value
    assert v[("andhra_pradesh", "phonepe_count_utility")] == 862829
    assert v[("jammu_kashmir", "phonepe_count_p2p")] == 46
    assert v[("jammu_kashmir", "phonepe_count_retail")] == 54
    assert v[("jammu_kashmir", "phonepe_count_utility")] == 10
    assert list(df.columns) == ["state_id", "period", "variable", "value", "source"]


def test_zero_fill(tmp_path):
    _, chk = build_types(FIX, tmp_path / "t.csv", tmp_path / "c.csv", ["2018Q1"])
    z = chk.set_index("check").loc["absent_type_set_to_zero"]
    assert z.n_items == 9 and z.n_flagged == 1 and z.note == "ladakh 2018Q1 Utility"
    assert chk.check.tolist() == ["type_sum_equals_state_total", "absent_type_set_to_zero",
                                  "national_count_equals_state_sum", "national_users_equals_state_sum"]


def test_sum_mismatch_raises(tmp_path):
    repo = _copy(tmp_path)
    def f(t): t[0]["paymentInstruments"][0]["count"] = 4000001
    _edit(repo / AGG / "state/andhra-pradesh/2018/1.json", f)
    with pytest.raises(ValueError, match="andhra-pradesh/2018/1.json"):
        _run(repo, tmp_path)


def test_absent_type_bad_sum_raises(tmp_path):
    repo = _copy(tmp_path)
    def f(t): t[1]["paymentInstruments"][0]["count"] = 3
    _edit(repo / AGG / "state/ladakh/2018/1.json", f)
    with pytest.raises(ValueError, match="ladakh/2018/1.json"):
        _run(repo, tmp_path)


def test_national_mismatch_raises(tmp_path):
    repo = _copy(tmp_path)
    def f(t): t[2]["paymentInstruments"][0]["count"] = 4000047
    _edit(repo / AGG / "2018/1.json", f)
    with pytest.raises(ValueError, match="india/2018/1.json"):
        _run(repo, tmp_path)


def test_unknown_type_raises(tmp_path):
    repo = _copy(tmp_path)
    def f(t): t[0]["name"] = "Other"
    _edit(repo / AGG / "state/andhra-pradesh/2018/1.json", f)
    with pytest.raises(ValueError, match="Other"):
        _run(repo, tmp_path)


@pytest.mark.skipif(not REAL.exists(), reason="raw clone absent")
def test_real_types_sum_to_count():
    t = pd.read_csv("data/interim/phonepe_types.csv")
    p = pd.read_csv("data/interim/phonepe.csv")
    assert len(t) == 3570 and t.state_id.nunique() == 35
    s = t.groupby(["state_id", "period"]).value.sum()
    c = p[p.variable == "phonepe_count"].set_index(["state_id", "period"]).value
    assert (s - c.loc[s.index]).abs().max() == 0
