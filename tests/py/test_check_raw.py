from btpdata.check_raw import missing, url_for


def test_missing_reports_url(tmp_path):
    d = tmp_path / "cpi"
    d.mkdir()
    (d / "manifest.csv").write_text("file,url\na.json,https://example.org/a\n", encoding="utf-8")
    msgs = missing([d / "a.json"])
    assert len(msgs) == 1 and "https://example.org/a" in msgs[0] and "a.json" in msgs[0]


def test_present_is_ok(tmp_path):
    f = tmp_path / "a.json"
    f.write_text("{}")
    assert missing([f]) == []
