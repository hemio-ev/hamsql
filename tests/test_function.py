from utils import *

def test_functions():

    xs = {
     'domains':
     [
      ('hamsql-test', 'user', 'function-install', 'varchar', 10, None) ,
     ],
     'functions':
     [
      ('function-install', 'f1', [], 'character varying', None, False) ,
      ('function-install', 'f2', [], 'character varying', None, False) ,
      ('public', 'f', [], 'integer', None, False) ,
     ]
    }

    runAssertSilent('install', 'function.yml', delete_db=True)
    check(**xs)

    runAssertSilent('upgrade', 'function.yml')
    check(**xs)

    xs['functions'] = \
     [
      ('public', 'f', [], 'integer', None, False) ,
     ]

    runAssertSilent('upgrade', 'function-upgrade.yml')
    check(**xs)

