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

    compl = run('install', 'domain.yml', delete_db=True)
    assert compl.returncode == 0
    check(**xs)

    compl = run('install', 'domain.yml')
    assert compl.returncode == 1

    compl = run('install', 'domain.yml', delete_db=True)
    assert compl.returncode == 0
    check(**xs)

    compl = run('install', 'domain.yml', delete_db=True)
    assert compl.returncode == 0
    check(**xs)

    compl = run('upgrade', 'domain.yml')
    assert compl.returncode == 0
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

    compl = run('upgrade', 'domain-upgrade.yml')
    assert compl.returncode == 0
    check(**xs)

