$name$
======================================================================

$description$

.. contents:: Schema Contents
   :local:
   :depth: 2


$if(tables)$

Tables
------

$for(tables)$

.. _TABLE-$name$.$tables.name$:

$name$.$tables.name$
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

$tables.description$

Primary key
+++++++++++

$for(tables.primary_key)$
- $tables.primary_key$
$endfor$


$if(tables.inherits)$

Inherits 
++++++++

$for(tables.inherits)$
- $tables.inherits$
$endfor$
$endif$

Columns
+++++++

$for(tables.columns)$
.. _COLUMN-$name$.$tables.name$.$tables.columns.name$:
   
``$tables.columns.name$``
     $if(tables.columns.null)$*NULL* | $endif$:ref:`$tables.columns.type$ <DOMAIN-$tables.columns.type$>`

     $tables.columns.description$

$if(tables.columns.default)$
   Default
    .. code-block:: sql

     $tables.columns.default$
$endif$

$if(tables.columns.references)$
   References :ref:`$tables.columns.references$ <COLUMN-$tables.columns.references$>`
$endif$

$if(tables.columns.on_ref_delete)$
   On Delete: $tables.columns.on_ref_delete$
$endif$

$if(tables.columns.on_ref_update)$
   On Update: $tables.columns.on_ref_update$
$endif$

$endfor$

.. BEGIN FKs

$if(tables.foreign_keys)$
Foreign keys
++++++++++++
$for(tables.foreign_keys)$

$tables.foreign_keys.name$
   *Local Columns*

$for(tables.foreign_keys.columns)$
   - $tables.foreign_keys.columns$
$endfor$

   *Referenced Columns*

$if(tables.foreign_keys.ref_columns)$
$for(tables.foreign_keys.ref_columns)$
   - :ref:`$tables.foreign_keys.ref_table$.$tables.foreign_keys.ref_columns$ <COLUMN-$tables.foreign_keys.ref_table$.$tables.foreign_keys.ref_columns$>`
$endfor$
$else$
$for(tables.foreign_keys.columns)$
   - :ref:`$tables.foreign_keys.ref_table$.$tables.foreign_keys.columns$ <COLUMN-$tables.foreign_keys.ref_table$.$tables.foreign_keys.columns$>`
$endfor$
$endif$

$endfor$
$endif$

.. END FKs

$endfor$

$endif$


$if(functions)$

Functions
---------

$for(functions)$


.. _FUNCTION-$name$.$functions.name$:

$name$.$functions.name$
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

$functions.description$

Returns
 :ref:`$functions.returns$ <DOMAIN-$functions.returns$>`

$if(functions.returns_columns)$
Returned Columns
$for(functions.returns_columns)$
 - ``$functions.returns_columns.name$`` :ref:`$functions.returns_columns.type$ <DOMAIN-$functions.returns_columns.type$>`
    $functions.returns_columns.description$
$endfor$
$endif$

$if(functions.language)$
Language
 $functions.language$
$endif$

$if(functions.parameters)$
Parameters 
++++++++++
$for(functions.parameters)$
 - ``$functions.parameters.name$`` :ref:`$functions.parameters.type$ <DOMAIN-$functions.parameters.type$>`
   $if(functions.variables.default)$(default: ``$functions.parameters.default$``)$endif$
    $functions.parameters.description$
$endfor$
$else$
 *None*
$endif$

$if(functions.variables)$
Variables
+++++++++
$for(functions.variables)$
 - ``$functions.variables.name$`` :ref:`$functions.variables.type$ <DOMAIN-$functions.variables.type$>`
   $if(functions.variables.default)$(default: ``$functions.variables.default$``)$endif$
   $functions.variables.description$
$endfor$
$endif$

$if(functions.priv_execute)$
Execute Privilege
+++++++++++++++++
$for(functions.priv_execute)$
 - :ref:`$functions.priv_execute$ <ROLE-$functions.priv_execute$>`
$endfor$
$endif$

Code
++++

.. code-block:: $if(functions.language)$guess$else$plpgsql$endif$

   $functions.body$

$endfor$

$endif$


$if(domains)$

Domains
-------

$for(domains)$

.. _DOMAIN-$name$.$domains.name$:

$name$.$domains.name$
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

$domains.description$

$if(domains.checks)$
Checks
++++++
$for(domains.checks)$
$domains.checks.name$
   $domains.checks.description$

   .. code-block:: sql

    $domains.checks.check$

$endfor$
$endif$

$endfor$
$endif$


$if(types)$

Types
-----

$for(types)$

.. _DOMAIN-$name$.$types.name$:

$name$.$types.name$
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

$types.description$

$endfor$
$endif$


$if(roles)$

Roles
-----

$for(roles)$

.. _ROLE-$roles.name$:

$roles.name$
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

$roles.description$

Login
 *$if(roles.login)$Enabled$else$Disabled$endif$*

$endfor$
$endif$


$if(sequences)$ 

Sequences
---------

$for(sequences)$

.. _SEQUENCE-$name$.$sequences.name$:

$name$.$sequences.name$
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

$sequences.description$

$endfor$
$endif$

.. This file was generated via HamSql

