import psycopg2
import subprocess
import time

dburl = "postgres://postgres@/hamsql-test"

def db_open():
    global dburl
    conn = psycopg2.connect(dburl)
    cur = conn.cursor()
    return conn, cur
    
def db_close(conn, cur):
    cur.close()
    conn.close()

def check(domains=[], functions=[], tables=[]):
    conn, cur = db_open()
    assert sorted(domains) == sorted(db_domains(cur))
    assert sorted(functions) == sorted(db_functions(cur))
    assert sorted(tables) == sorted(db_tables(cur))
    db_close(conn, cur)

def runAssertSilent(cmd, setup, **xs):
    completedProcess = run(cmd, setup, capture=True, **xs)
    assertSilent(completedProcess)
    return completedProcess

def assertSilent(completedProcess):
    assert completedProcess.returncode == 0
    assert completedProcess.stdout == ""
    assert completedProcess.stderr == ""

def assertError(completedProcess, err):
    assert completedProcess.returncode == 1
    assert completedProcess.stdout == ""
    assert err in completedProcess.stderr
    
def db_domains(cur):
    cur.execute("""
        SELECT domain_catalog, domain_name, domain_schema, udt_name, character_maximum_length, domain_default
            FROM information_schema.domains
            WHERE domain_schema <> 'information_schema'
        """)
    return cur.fetchall()
    
def db_tables(cur):
    cur.execute("""
        SELECT table_schema, table_name, table_type
            FROM information_schema.tables
            WHERE table_schema NOT IN ('information_schema', 'pg_catalog')
        """)
    return cur.fetchall()

def db_functions(cur):
    cur.execute("""
        SELECT
            n.nspname
            ,p.proname
            ,ARRAY(SELECT UNNEST(p.proargtypes::regtype[]::varchar[]))
            ,prorettype::regtype::varchar
            ,proargnames
            ,prosecdef
        FROM pg_catalog.pg_proc AS p
            JOIN pg_namespace AS n ON p.pronamespace = n.oid AND
                NOT n.nspname LIKE 'pg_%' AND
                n.nspname NOT IN ('information_schema')
            WHERE p.probin IS NULL
        """)
    return cur.fetchall()

def run(cmd, setup, delete_db=False, capture=False):
    global dburl
    settings = {}
    params = ['hamsql', cmd, '-s', setup, '-c', dburl]
    
    if delete_db:
        params += [ '--delete-existing-database' ]
    
    if capture:
        settings.update({
            'stdout': subprocess.PIPE,
            'stderr': subprocess.PIPE,
            'universal_newlines': True
        })
    
    return subprocess.run(params, **settings)

