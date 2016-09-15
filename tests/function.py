import utils

t = utils.test("postgres://postgres@/hamsql-test", "function.yml")

t.domains = \
 [
  ('hamsql-test', 'user', 'function-install', 'varchar', 10, None) ,
 ]

t.functions = \
 [
  ('function-install', 'f1', [], 'character varying', None, False) ,
  ('function-install', 'f2', [], 'character varying', None, False) ,
  ('public', 'f', [], 'integer', None, False) ,
 ]

t.run_cmd('install', delete_db=True)
t.check()

t.run_cmd('upgrade')
t.check()


t.functions = \
 [
  ('public', 'f', [], 'integer', None, False) ,
 ]
t.config = 'function-upgrade.yml'
t.run_cmd('upgrade')
t.check()

t.evaluate()

