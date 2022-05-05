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

Describe the database connection parameters in a file *conn.scm* file as
shown below. Take care to replace the placeholders within angle brackets
with the appropriate values.

``` scheme
((username . "<username-here>")
 (password . "<password-here>")
 (database . "<database-name-here>")
 (host . "<hostname-here>")
 (port . <port-here>))
```

Then, to dump the database to \~/data/dump, run

``` shell
$ ./pre-inst-env ./dump.scm conn.scm ~/data/dump
```

Make sure there is enough free space! It\'s best to dump the database on
penguin2 where disk space and bandwidth are not significant constraints.

# Contributing

See bugs and tasks in BUGS.org.
