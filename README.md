HamSql
======

An interpreter for SQL structure definitions in YAML ([YamSql](http://yamsql.readthedocs.io/))

[![build status](https://git.hemio.de/hemio/hamsql/badges/master/build.svg)](https://git.hemio.de/hemio/hamsql/commits/master)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/hamsql.svg?maxAge=2592000)](https://hackage.haskell.org/package/hamsql)
[![Hackage](https://img.shields.io/hackage/v/hamsql.svg?maxAge=2592000)](https://hackage.haskell.org/package/hamsql)

## About HamSql

HamSql is a software that parses SQL structures defined in a YAML based language and deploys them on PostgreSQL servers. It allows to maintain PostgreSQL projects in a form more similar to other programming languages.

In contrast to the `CREATE OR REPLACE FUNCTION` approaches, residual structures are deleted, column properties are deleted without explicit definition of the migration and the ordering imposed by dependencies is resolved automatically.

HamSql is on hackage, which means you can build it with `cabal install hamsql`. Please note the build requirements and details at the bottom. HamSql binaries for Linux amd64 are available as [build artifacts](https://git.hemio.de/hemio/hamsql/tags) from our build server.

**HamSql can be used for**

- Neat SQL development with clearer versioning via "one object one file" principle
- More flexible development with features like function and table templates
- On site deployments of SQL structures
- Off-line computation of upgrade strategies for known status quo
- Documentation generation of SQL structures and APIs


### What is there

- `hamsql install` command that deploys the defined structure
- `hamsql upgrade` command that updates existing structures
  - Per default no data are at risk during upgrade (no column/table deletion)
  - Overwrite via `--permit-data-deletion` possible
  - SQL command ordering that avoids dependency conflicts
  - Complicated dependencies are resolved via trial and error
- `hamsql doc` creates a documentation of the complete sql structure
  - Custom templates can be provided using [doctemplates](https://hackage.haskell.org/package/doctemplates) known from pandoc
  - The build-in template creates [Sphinx and Read the Docs](https://docs.readthedocs.io) ready *.rst* files
- Code basis tailored for the support of many SQL features
- [Basic documentation of the YamSql Language](http://yamsql.readthedocs.io)

### Coming soon

- Support for views and triggers
- Warn about name conflicts for SQL objects before deployment
- Output defining YamSql file for each type of error

### What is missing

Those are all things on the radar but the exact requirements are unclear or the workforce is missing.

- [Support for several other PostgreSQL features](https://git.hemio.de/hemio/hamsql/issues?milestone_title=Support+all+SQL+Features)
- Stable definition of YamSql
- Support for renaming tables and columns
- Covering other strategies required for upgrade
- Unit and integration test as part of YamSql
  - Define scenarios that are loaded into the database
  - Define test and expected result for a function for a scenario
  - Define operations with expected outcome (to tests triggers etc.)

## Example Project

The example project below could be deployed via

    hamsql install -c postgresql:///dbname

Later changes can be pushed via

    hamsql upgrade -c postgresql:///dbname

The default documentation can be written to *docs/* via

    hamsql doc

You can have a look at the [output rendered via Sphinx.](http://yamsql-example-project.readthedocs.io)

Those are the YamSql files for the project:

```yaml
# setup.yml
schemas:
 - math
schema_dirs:
 - schemas
```

```yaml
# schema/math/schema.yml
name: math
description: |
 Some basic math in SQL
```

```yaml
---
# schema/math/function.d/factorial.plsql
name: factorial
description: |
 Factorial function using the
 ``WITH RECURSIVE`` SQL feature.

 Logical definition::

     f(0) = 1
     f(n) = n * f (n - 1)

parameters:
 - name: p_n
   type: int
returns: int
---
RETURN (
  WITH RECURSIVE t AS (
      SELECT 1 AS f, 0 AS n
    UNION ALL
      SELECT f * (n + 1), n + 1 FROM t
  )
  SELECT f FROM t WHERE n=p_n LIMIT 1
);
```

```yaml
---
# schema/math/function.d/erf.py
name: erf 
description: |
 Gauss error function

 This function is a wrapper for the
 Python 3 implementation.

parameters:
 - name: x
   type: float
returns: float

language: plpython3u
---
import math
return math.erf(x)

```

## Building HamSql on Debian Stretch

To completely build HamSql from source

    apt install make ghc cabal-install libpq-dev happy
    make
    make install
    
To avoid compiling all the dependencies you can use the following set of debian packages instead of the above ones

```sh
apt install \
 make \
 ghc \
 cabal-install \
 libghc-aeson-dev \
 libghc-file-embed-dev \
 libghc-network-uri-dev \
 libghc-optparse-applicative-dev \
 libghc-pandoc-dev \
 libghc-postgresql-simple-dev \
 libghc-text-dev \
 libghc-unordered-containers-dev \
 libghc-yaml-dev
```

