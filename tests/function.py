import utils

t = utils.test("postgres://postgres@/hamsql-test", "function.yml")

t.domains = \
 [
  ('hamsql-test', 'user', 'function-install', 'varchar', 10, None) ,
 ]

t.run_cmd('install', delete_db=True)
t.check()

t.evaluate()

