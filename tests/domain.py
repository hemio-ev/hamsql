import utils

t = utils.test("postgres://postgres@/hamsql-test", "domain.yml")

t.domains = \
 [
  ('hamsql-test', 'A1', 'domain-install', 'int4', None, None) ,
  ('hamsql-test', 'A2', 'domain-install', 'A1', None, None) ,
  ('hamsql-test', 'B1', 'domain-install', 'int4', None, None) ,
  ('hamsql-test', 'L1', 'domain-install', 'varchar', 25, None) ,
  ('hamsql-test', 'D1', 'domain-install', 'varchar', None, None) ,
  ('hamsql-test', 'B2', 'domain-install', 'B1', None, None) ,
 ]

t.tables = \
 [
  ('domain-install', 't1', 'BASE TABLE') ,
 ]

t.run_cmd('install', returncode=1, delete_db=False)

t.run_cmd('install', delete_db=True)
t.check()

t.run_cmd('install', delete_db=True)
t.check()

t.run_cmd('upgrade')
t.check()

t.evaluate()

