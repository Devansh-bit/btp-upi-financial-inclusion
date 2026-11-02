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


def _manual_file(tmp_path, content: bytes, manifest_sha: str):
    d = tmp_path / "npci"
    d.mkdir()
    f = d / "a.json"
    f.write_bytes(content)
    (d / "manifest.csv").write_text(
        f"file,url,retrieved,sha256,notes\na.json,https://example.org/a,2026-11-01,{manifest_sha},\n",
        encoding="utf-8")
    return f


def test_sha256_match_is_ok(tmp_path):
    import hashlib
    from btpdata.check_raw import hash_mismatches
    f = _manual_file(tmp_path, b"{}", hashlib.sha256(b"{}").hexdigest())
    assert hash_mismatches([f]) == []


def test_sha256_mismatch_names_file_and_hashes(tmp_path):
    import hashlib
    from btpdata.check_raw import hash_mismatches
    f = _manual_file(tmp_path, b"{}", "0" * 64)
    msgs = hash_mismatches([f])
    assert len(msgs) == 1
    assert str(f) in msgs[0] and "0" * 64 in msgs[0] and hashlib.sha256(b"{}").hexdigest() in msgs[0]


def test_sha256_without_manifest_value_fails(tmp_path):
    from btpdata.check_raw import hash_mismatches
    f = _manual_file(tmp_path, b"{}", "")
    msgs = hash_mismatches([f])
    assert len(msgs) == 1 and "a.json" in msgs[0]


def test_main_exits_non_zero_on_mismatch(tmp_path, capsys):
    import pytest
    from btpdata.check_raw import main
    f = _manual_file(tmp_path, b"{}", "0" * 64)
    with pytest.raises(SystemExit) as e:
        main([f])
    assert e.value.code not in (0, None)
    assert "0" * 64 in str(e.value.code)


def test_main_ok_when_hash_matches(tmp_path, capsys):
    import hashlib
    from btpdata.check_raw import main
    f = _manual_file(tmp_path, b"{}", hashlib.sha256(b"{}").hexdigest())
    main([f])
    assert "present" in capsys.readouterr().out
