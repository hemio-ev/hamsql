from utils import *

def test_domains():

    xs = {
     'domains':
     [
      ('hamsql-test', 'dep', 'domain-dependency', 'int4', None, None) ,
      ('hamsql-test', 'A1', 'domain-install', 'int4', None, None) ,
      ('hamsql-test', 'A2', 'domain-install', 'A1', None, None) ,
      ('hamsql-test', 'B1', 'domain-install', 'int4', None, None) ,
      ('hamsql-test', 'L1', 'domain-install', 'varchar', 25, None) ,
      ('hamsql-test', 'D1', 'domain-install', 'varchar', None, None) ,
      ('hamsql-test', 'B2', 'domain-install', 'B1', None, None) ,
     ],

     'tables':
     [
      ('domain-install', 't1', 'BASE TABLE') ,
     ]
    }

    runAssertSilent('install', 'domain.yml', delete_db=True)
    check(**xs)

    compl = run('install', 'domain.yml', capture=True)
    assertError(compl, "42P04")

    runAssertSilent('install', 'domain.yml', delete_db=True)
    check(**xs)

    runAssertSilent('install', 'domain.yml', delete_db=True)
    check(**xs)

    runAssertSilent('upgrade', 'domain.yml')
    check(**xs)

    xs.update({
     'domains':
      [
       ('hamsql-test', 'A1', 'domain-install', 'int4', None, None) ,
       ('hamsql-test', 'A2', 'domain-install', 'A1', None, None) ,
       ('hamsql-test', 'B1', 'domain-install', 'int4', None, None) ,
       ('hamsql-test', 'B2', 'domain-install', 'B1', None, None) ,
       ('hamsql-test', 'D2', 'domain-install', 'varchar', None, None) ,
       ('hamsql-test', 'L1', 'domain-install', 'varchar', 25, None) ,
       ('hamsql-test', 'dep', 'domain-dependency', 'int4', None, None) ,
      ]
    })

    runAssertSilent('upgrade', 'domain-upgrade.yml')
    check(**xs)

