from utils import *

def test_roles():

    xs = {
     'roles':
     [
      ('hamsql-test_group1', False, True, False, False, False, -1, False, None),
      ('hamsql-test_role1', False, True, False, False, False, -1, False, None)
     ]
    }

    runAssertSilent('install', 'role.yml', delete_db=True)
    check(**xs)

    runAssertSilent('upgrade', 'role.yml')
    check(**xs)

    runAssertSilent('upgrade', 'nothing.yml')
    check()

