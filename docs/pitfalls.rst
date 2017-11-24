Pitfalls
========

Changed function return type
----------------------------

Solution: ``DROP FUNCTION`` before redefinition if return type differs

.. code-block:: sql

    DROP FUNCTION IF EXISTS f();
    CREATE FUNCTION f()
        RETURNS integer LANGUAGE SQL AS 'SELECT 0';
    CREATE OR REPLACE FUNCTION f()
        RETURNS varchar LANGUAGE SQL AS 'SELECT 0::varchar';
    -- ERROR:  cannot change return type of existing function
    -- HINT:  Use DROP FUNCTION f() first.

Owned sequences
---------------

Solution: ``DROP DEFAULT`` before ``DROP SEQUENCE``

.. code-block:: sql

    DROP TABLE IF EXISTS t;
    DROP SEQUENCE IF EXISTS t_t1_seq;
    CREATE TABLE t (t1 integer);
    CREATE SEQUENCE t_t1_seq OWNED BY t.t1;
    DROP SEQUENCE t_t1_seq;
    CREATE SEQUENCE t_t1_seq OWNED BY t.t1;
    ALTER TABLE t ALTER COLUMN t1 SET DEFAULT nextval('t_t1_seq'::regclass);
    DROP SEQUENCE t_t1_seq;
    -- ERROR:  cannot drop sequence t_t1_seq because other objects depend on it
    -- DETAIL:  default for table t column t1 depends on sequence t_t1_seq
    -- HINT:  Use DROP ... CASCADE to drop the dependent objects too.
    DROP TABLE t;
    DROP SEQUENCE t_t1_seq;
    -- ERROR:  sequence "t_t1_seq" does not exist

The first error does not depend on the ``OWNED BY`` property. The second does, since otherwise the sequence wouldn't be deleted.

Type limits are irrelevant for functions
----------------------------------------

Solution: Distinquish between function and column types

.. code-block:: sql

    DROP FUNCTION IF EXISTS g(varchar);
    CREATE FUNCTION g(varchar(2))
        RETURNS varchar(3) LANGUAGE SQL AS 'SELECT $1';
    SELECT g('abcd');
    -- abcd (1 row)

    DROP FUNCTION IF EXISTS g(time);
    CREATE FUNCTION g(time (1))
        RETURNS time (2) LANGUAGE SQL AS 'SELECT $1';
    SELECT g('12:11:10.12345'::time);
    -- 12:11:10.12345 (1 row)
    CREATE OR REPLACE FUNCTION g(time (1))
        RETURNS time (2) LANGUAGE SQL AS 'SELECT $1::time (3)';
    SELECT g('12:11:10.12345'::time);
    -- 12:11:10.123 (1 row)

    DROP TABLE IF EXISTS s;
    CREATE TABLE s (s1 varchar(2), s2 time(2));
    INSERT INTO s VALUES ('abcd', '12:11:10.12345');
    -- ERROR:  value too long for type character varying(2)
    INSERT INTO s VALUES ('ab', '12:11:10.12345');
    SELECT t2 FROM t;
    SELECT s2 FROM s;
    -- 12:11:10.12 (1 row)