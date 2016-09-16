from utils import *

def test_tables():

    xs = {
     'tables':
     [
      ('table-install', 't1', 'BASE TABLE') ,
     ]
    }

    runAssertSilent('install', 'table.yml', delete_db=True)
    check(**xs)

    runAssertSilent('upgrade', 'table.yml')
    check(**xs)

