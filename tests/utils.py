import psycopg2
import subprocess
import sys

def print_arr(data):
    print("var = \\\n [")
    for tpl in data:
        print(" ", tpl, ",")
    print(" ]")

def indent(text):
    return ('\n' + text).replace('\n', '\n  ')

class test:
    def __init__(self, url, config):
        self.cur = None
        self.code = 0
        self.url = url
        self.config = config
        
        self.domains = []
        self.tables = []
        self.functions = []

    def run_cmd(self, cmd, returncode=0, delete_db=False):
        params = ['hamsql', cmd, '-s', self.config, '-c', self.url]
        
        if delete_db:
            params += [ '--delete-existing-database' ]

        print("---")
        r = subprocess.run(params, stderr=subprocess.PIPE)
        
        if r.returncode != returncode:
            print("ERROR", indent(r.stderr.decode('utf-8')))
            if returncode:
                self.code = r.returncode
            else:
                self.code = 255

        print("---")

    def check(self):
        conn = psycopg2.connect(self.url)
        self.cur = conn.cursor()
        
        self.assert_seteq("domains", self._get_domains(), self.domains)
        self.assert_seteq("tables", self._get_tables(), self.tables)
        self.assert_seteq("functions", self._get_functions(), self.functions)
        
        self.cur.close()
        conn.close()
        self.cur = None

    def assert_seteq(self, name, a, b):
        if (sorted(a) != sorted(b)):
            print(name + " failed.")
            self.code = 254
            print("actual")
            print_arr(sorted(a))
            print("expected")
            print_arr(sorted(b))
        else:
            print(name + " done.")

    def _get_domains(self):
        self.cur.execute("""
            SELECT domain_catalog, domain_name, domain_schema, udt_name, character_maximum_length, domain_default
                FROM information_schema.domains
                WHERE domain_schema <> 'information_schema'
            """)
        return self.cur.fetchall()
        
    def _get_tables(self):
        self.cur.execute("""
            SELECT table_schema, table_name, table_type
                FROM information_schema.tables
                WHERE table_schema NOT IN ('information_schema', 'pg_catalog')
            """)
        return self.cur.fetchall()

    def _get_functions(self):
        self.cur.execute("""
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
        return self.cur.fetchall()
                
    def evaluate(self):
        if self.code:
            print("FAILED TESTS PRESENT")
        else:
            print("TESTS PASSED")
        sys.exit(int(self.code))

