from utils import *
import os
import os.path
import shutil

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

def test_doc():
    out = 'docs/'
    
    if os.path.exists(out):
        shutil.rmtree(out)
    os.mkdir(out)
    
    runAssertSilent('doc', 'function.yml')

    assert os.path.exists('docs/function-install.rst')
    assert sum(1 for line in open('docs/function-install.rst')) > 70

