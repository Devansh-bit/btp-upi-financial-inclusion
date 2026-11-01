import pandas as pd

from btpdata.validation import compare


def panel(states, counts, period="2025Q2"):
    return pd.DataFrame({"state_id": states, "period": period, "phonepe_count": counts,
                         "phonepe_amount_inr": [c * 10 for c in counts], "population": [100.0] * len(states)})


def npci(states, vols, period="2025Q2"):
    return pd.DataFrame({"state_id": states, "period": period, "npci_volume": vols,
                         "npci_value_inr": [v * 10 for v in vols]})


def test_perfect_agreement():
    s = ["a", "b", "c", "d"]
    r = compare(panel(s, [1, 2, 3, 4]), npci(s, [10, 20, 30, 40]))
    assert list(r.columns) == ["period", "measure", "pearson", "spearman", "n_states"]
    assert set(r.measure) == {"share_count", "share_value", "count_pc", "value_pc"}
    assert (r.pearson.round(9) == 1).all() and (r.spearman == 1).all()


def test_n_states_intersection():
    r = compare(panel(["a", "b", "c"], [1, 2, 3]), npci(["a", "b"], [1, 2]))
    assert (r.n_states == 2).all()


def test_only_common_periods():
    p = pd.concat([panel(["a", "b", "c"], [1, 2, 3], "2025Q2"), panel(["a", "b", "c"], [1, 2, 3], "2018Q1")])
    r = compare(p, npci(["a", "b", "c"], [1, 2, 3], "2025Q2"))
    assert set(r.period) == {"2025Q2"}


def test_shares_over_common_states():
    # PhonePe has 'x' that NPCI lacks: it must not enter the denominator
    from btpdata.validation import shares
    s = shares(pd.Series([10.0, 30.0, 60.0], index=["a", "b", "c"]))
    assert s.round(9).tolist() == [0.1, 0.3, 0.6]


def test_shares_are_scale_invariant():
    s = ["a", "b", "c", "d"]
    r = compare(panel(s + ["x"], [1, 2, 3, 4, 1000]), npci(s, [10, 20, 30, 40]))
    assert (r.n_states == 4).all()
    assert (r[r.measure == "share_count"].pearson.round(9) == 1).all()
