from utils import *

def test_tables():

    xs = {
     'tables':
     [
      ('table-install', 't1', 'BASE TABLE') ,
     ],
     'roles':
     [
        ('hamsql-test_group1', False, True, False, False, False, -1, False, None),
        ('hamsql-test_role1', False, True, False, False, False, -1, False, None)
     ]
    }

    runAssertSilent('install', 'table.yml', delete_db=True)
    check(**xs)

    runAssertSilent('upgrade', 'table.yml')
    check(**xs)

def test_permit_data_deletion():

    runAssertSilent('install', 'table.yml', delete_db=True)
    runAssertSilent('upgrade', 'domain.yml', args=['--permit-data-deletion'])

def test_missing_permit_data_deletion():
    # setup something with tables
    runAssertSilent('install', 'table.yml', delete_db=True)
   
    # Upgrade to something without tables
    # Should warn about "missing" --permit-data-deletion
    result = run('upgrade', 'domain.yml', capture=True)
    assertStdErr(result, "--permit-data-deletion")
    
    # With verbose a TABLE that cannot be deleted should be reported
    result = run('upgrade', 'domain.yml', args=['--verbose'], capture=True)
    assertStdErr(result, "TABLE")
    
def test_missing_database_deletion():
    runAssertSilent('install', 'table.yml', delete_db=True)
    
    result = run('install', 'table.yml', capture=True)
    assertError(result, "42P04")
    
