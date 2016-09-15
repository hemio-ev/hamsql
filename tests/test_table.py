from utils import *

def test_tables():

    xs = {
     'tables':
     [
      ('table-install', 't1', 'BASE TABLE') ,
     ]
    }

    compl = run('install', 'table.yml', delete_db=True)
    assert compl.returncode == 0
    check(**xs)

    compl = run('upgrade', 'table.yml')
    assert compl.returncode == 0
    check(**xs)

