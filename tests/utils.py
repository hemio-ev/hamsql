import subprocess
import psycopg2

dburl = "postgres://postgres@/hamsql-test"

def dbcur():
    global dburl
    conn = psycopg2.connect(dburl)
    cur = conn.cursor()
    return conn, cur
    
    #cur.close()
    #conn.close()


def check(domains=[], functions=[], tables=[]):
    conn, cur = dbcur()
    assert set(domains) == set(db_domains(cur))
    assert sorted(functions) == sorted(db_functions(cur))
    assert set(tables) == set(db_tables(cur))

def run(cmd, setup, delete_db=False):
    global dburl
    params = ['hamsql', cmd, '-s', setup, '-c', dburl]
    
    if delete_db:
        params += [ '--delete-existing-database' ]

    return subprocess.run(params)

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
