---
name: returns-table
description: Function that returns a table
language: plpgsql
returns: 
 table:
 - name: arg1
   type: int
 - name: arg2
   type: varchar
parameters:
 - name: x1
   type: character varying
---
BEGIN RETURN QUERY SELECT * FROM (VALUES (1, 'a'), (2, 'b'), (3, 'c')) AS x; END;
