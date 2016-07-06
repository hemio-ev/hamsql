import utils

t = utils.test("postgres://postgres@/hamsql-test", "table.yml")

t.tables = \
 [
  ('table-install', 't1', 'BASE TABLE') ,
 ]

t.run_cmd('install', delete_db=True)
t.check()

t.run_cmd('upgrade')
t.check()

t.evaluate()

