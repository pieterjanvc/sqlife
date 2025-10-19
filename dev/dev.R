dbInfo <- "local/test.db"
schema <- "tests/testthat/testdata/dummy1.sql"
dbSetup(dbInfo, schema, validateSchema = T)


dbPKlist <- function(dbInfo, schema) {
  if (missing(schema)) {
    conn <- dbGetConn(dbInfo)
  } else {
    conn <- dbNewFromSchema(
      schema = schema,
      memory = ":memory:",
      returnConn = T
    )$conn
  }

  on.exit(dbFinish(conn, commit = F, showWarnings = F))
  tables <- dbListTables(conn)
  tables <- tables[!tables %in% "sqlite_sequence"]

  setNames(
    lapply(tables, function(table) {
      dbGetQuery(conn, sprintf("PRAGMA table_info('%s');", table)) |>
        filter(pk == 1) |>
        pull(name)
    }),
    tables
  )
}

dbPKlist(dbInfo)
