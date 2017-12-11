---
name: sreturns-table-1col
description: Function that returns a table
language: plpgsql
returns:
 table:
   - name: arg1
     type: int
parameters:
 - name: x1
   type: character varying
---
BEGIN RETURN QUERY SELECT * FROM (VALUES (1), (2), (3)) AS x; END;
