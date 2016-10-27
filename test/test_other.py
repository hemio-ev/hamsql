from utils import *

def test_check_syntax_first_install():
    run('install', 'function.yml', delete_db=True)
    compl = run('install', 'invalid.yml', capture=True)

    # duplicate_database means that we didn't catched the syntax error
    assert "42P04" not in compl.stderr

    assert "invalid/schema.yml" in compl.stderr

def test_check_syntax_first_upgrade():
    run('install', 'function.yml', delete_db=True)

    compl = run('upgrade', 'invalid.yml', capture=True, invalid_connection=True)

    assert "invalid/schema.yml" in compl.stderr
