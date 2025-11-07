# sqlife

#### _Extend RSQLite with higher level functions for common database usecases_

This package is aimed as people who regularly use SQLite databases in R but
would like some higher level functions to more quickly create, maintain and
transact with SQLite databases.

## Installing

Install the package manually from GitHub

```r
devtools::install_github("pieterjanvc/sqlife", ref = "v0.1.1")
```

- Use `ref` for installing a
  [release version](https://github.com/pieterjanvc/sqlife/releases), or omit
  this for using the most recent but less tested main branch

## Connecting and Disconnecting

### dbSetup - Setup / check a database

The `dbSetup` function will create / check an SQLite database and return a list
with info about success / failure (does not throw hard stop errors)

```r
path <- "example.db"
schema <- system.file("example.sql", package = "sqlife")
dbSetup(path, schema, validateSchema = T)
```

#### Scenario 1 - Existing database

In this case `dbSetup` will check if there is a database at the given path and
if `validateSchema = T` will check that the schema matches the provided one.
This is especially helpful during dev when the database can change.

#### Scenario 1 - No existing database

In this case `dbSetup` will automatically create a new database if a valid
schema is provided and `createNew = T` (default).

### dbGetConn - Get a database connection

The `dbGetConn` function will take any of the following

- path to an SQLite database -> new connection will be checked out
- existing database connection -> passed on
- pool object (pool package) -> new connection will be checked out

An will return a connection

```r
dbInfo <- "example.db"
dbGetConn(dbInfo, startTransaction = F, enforceKeyConstraints = T)
```

- Key constraints are enforced by default for a connection opened this way
- If `startTransaction = T` this will start a new transaction instead of
  auto-committing changes

### dbFinish - Finish a database interaction

```r
dbFinish(conn, commit = T, closeExisting = F)
```

- If `attr(conn, "existing")` is `FALSE`, the connection will automatically be
  closed otherwise it remains open unless `closeExisting` is `TRUE`
- If there is an ongoing transaction, `commit` will either commit it or roll
  back
- If the `error` argument is set, the database will roll back (if transacting)
  and close before throwing an error with the content provided
  
#### Important note
You must run `dbFinish` before the exiting the environment where you opened
the connection using `dbGetConn` or you will get an error. This enforces best
practice of deciding how to handle any remaining commits and will also ensure
that upon error the database is always rolled back and closed so it won't be
locked or have a corrupt journal.

## Data manipulation

### tbl_insert - Insert into and existing table

This function will take a dataframe and insert it into a table in a database
(dbInfo). For this to work, the table must have all columns that make up the
primary key (unless they are auto-incrementing) and columns that can't be empty.

```r
tbl_insert(dataframe, dbInfo, table, commit = T)
```

- If `commit = F`, a transaction will be started (or continued). This only works
  when `dbInfo` is an _existing_ connection, otherwise the insertion is
  automatically committed
- In case of an error, the any open transaction is rolled back and the
  connection is closed 

### tbl_update - Update a table

This function will take a dataframe and use it to update a table in a database
(dbInfo). For this to work, the table must have all columns that make up the
primary key (unless they are auto-incrementing). All additional columns provided
will be updated.

```r
tbl_update(dataframe, dbInfo, table, commit = T)
```

- If `commit = F`, a transaction will be started (or continued). This only works
  when `dbInfo` is an _existing_ connection, otherwise the insertion is
  automatically committed
- In case of an error, the any open transaction is rolled back and the
  connection is closed

### tbl_delete - Delete rows in a table

This function will take a dataframe and use it to delete rows in a table in the
database (dbInfo). For this to work, the table must have all columns that make
up the primary key. All additional columns provided will be ignored.

```r
tbl_delete(dataframe, dbInfo, table, commit = T)
```

- If `commit = F`, a transaction will be started (or continued). This only works
  when `dbInfo` is an _existing_ connection, otherwise the insertion is
  automatically committed
- In case of an error, the any open transaction is rolled back and the
  connection is closed
