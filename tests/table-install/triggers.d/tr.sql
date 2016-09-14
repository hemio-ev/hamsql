name: tr1
description: Trigger 1
tables: 
 - table-install.t1
moment: AFTER
events:
 - INSERT
 - UPDATE
for_each: ROW
body: |
 RAISE EXCEPTION 'what ever …';
