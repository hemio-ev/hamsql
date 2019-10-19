---
name: f
description: Function f 1 arg
language: plpgsql

grant:
 - role: [myrole]
   privilege: [EXECUTE]

returns: int
parameters:
 - name: x1
   type: character varying
---
BEGIN RETURN 5; END;
