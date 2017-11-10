---
name: f
description: Function f
language: plpgsql
returns: integer
parameters:
 - name: x1
   type: integer
 - name: x2
   type: integer
   default: '9'
 - name: x3
   type: character varying
   default: "'mydef,str'::character varying"
 - name: x4
   type: date
   default: now()
---
BEGIN RETURN x1*2 + x2; END;
