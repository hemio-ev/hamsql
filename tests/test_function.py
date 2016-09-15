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

    compl = run('install', 'function.yml', delete_db=True)
    assert compl.returncode == 0
    check(**xs)

    compl = run('upgrade', 'function.yml')
    assert compl.returncode == 0
    check(**xs)

    xs['functions'] = \
     [
      ('public', 'f', [], 'integer', None, False) ,
     ]

    compl = run('upgrade', 'function-upgrade.yml')
    assert compl.returncode == 0
    check(**xs)

