[![dump-genenetwork-database-tests CI
badge](https://ci.genenetwork.org/badge/dump-genenetwork-database-tests.svg)](https://ci.genenetwork.org/jobs/dump-genenetwork-database-tests)
[![dump-genenetwork-database CI
badge](https://ci.genenetwork.org/badge/dump-genenetwork-database.svg)](https://ci.genenetwork.org/jobs/dump-genenetwork-database)

The GeneNetwork database is being migrated from a relational database to
a plain text and RDF database. This repository contains code to dump the
relational database to plain text.

# Using

Drop into a development environment with

``` shell
$ guix shell
```

Build the sources.

``` shell
$ make
```

## Set up connection parameters

Describe the database connection parameters in a file *conn.scm* file as
shown below. Take care to replace the placeholders within angle brackets
with the appropriate values.

``` scheme
((generif-data-file . "/path/to/generifs_basic.gz")
 (sql-username . "<sql-username-here>")
 (sql-password . "<sql-password-here>")
 (sql-database . "<sql-database-name-here>")
 (sql-host . "<sql-hostname-here>")
 (sql-port . <sql-port-here>)
 (virtuoso-port . <virtuoso-port-here>)
 (virtuoso-username . "<virtuoso-username-here>")
 (virtuoso-password . "<virtuoso-password-here>")
 (sparql-scheme . <sparql-endpoint-scheme-here>)
 (sparql-host . "<sparql-endpoint-hostname-here>")
 (sparql-port . <sparql-endpoint-port-here>))
```

Download the GeneRIF data file from
https://ftp.ncbi.nih.gov/gene/GeneRIF/generifs_basic.gz and specify
its path in the `generif-data-file` parameter.

Here's a sample *conn.scm*.
``` scheme
((generif-data-file . "/home/gn/generifs_basic.gz")
 (sql-username . "webqtlout")
 (sql-password . "my-secret-password")
 (sql-database . "db_webqtl")
 (sql-host . "localhost")
 (sql-port . 3306)
 (virtuoso-port . 9081)
 (virtuoso-username . "dba")
 (virtuoso-password . "my-secret-virtuoso-password")
 (sparql-scheme . http)
 (sparql-host . "localhost")
 (sparql-port . 9082))
```

## Dump the database

Then, to dump the database to \~/data/dump, run

``` shell
$ ./pre-inst-env ./dump.scm conn.scm ~/data/dump
```

Make sure there is enough free space! It\'s best to dump the database on
penguin2 where disk space and bandwidth are not significant
constraints.

## Validate and load dump

Then, validate the dumped RDF using `rapper` and load it into
virtuoso. This will load the dumped RDF into the
`http://genenetwork.org` graph, and will delete all pre-existing data
in that graph.

``` shell
$ rapper --input turtle --count ~/data/dump/dump.ttl
$ ./pre-inst-env ./load-rdf.scm conn.scm ~/data/dump/dump.ttl
```

## Visualize schema

Now, you may query virtuoso to visualize the SQL and RDF schema.

``` shell
$ ./pre-inst-env ./visualize-schema.scm conn.scm
```

This will output graphviz dot files `sql.dot` and `rdf.dot` describing
the schema. Render them into SVG images like so.

``` shell
$ dot -Tsvg -osql.svg sql.dot
$ dot -Tsvg -ordf.svg rdf.dot
```

Or, peruse them interactively with `xdot`.

``` shell
$ xdot sql.dot
$ xdot rdf.dot
```

The
[dump-genenetwork-database](https://ci.genenetwork.org/jobs/dump-genenetwork-database)
continuous integration job runs these steps on every commit and
publishes its version of
[sql.svg](https://ci.genenetwork.org/archive/dump-genenetwork-database/latest/sql.svg)
and
[rdf.svg](https://ci.genenetwork.org/archive/dump-genenetwork-database/latest/rdf.svg).

# Contributing

See bugs and tasks in BUGS.org.
