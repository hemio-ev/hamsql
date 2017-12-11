---
name: simple_table_returned
description: SETOF function
language: plpgsql
returns:
  setof: self-test.simple
---
BEGIN
  RETURN QUERY SELECT * FROM "self-test".simple;
END;
