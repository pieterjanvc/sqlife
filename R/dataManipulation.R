tbl_insert <- function(dataframe, dbInfo, table, commit = T) {
  if (class(dbInfo) == "character" & !commit) {
    stop(
      "Only existing connections can have commit = F",
      "Use dbGetConn() to open a connection first"
    )
  }

  conn <- dbGetConn(dbInfo)

  if (!sqliteIsTransacting(conn)) {
    dbBegin(conn)
  }

  tryCatch(
    {
      result <- dbGetQuery(
        conn,
        sprintf(
          'INSERT INTO "%s"("%s") VALUES(%s) RETURNING *',
          table,
          paste(colnames(dataframe), collapse = '","'),
          paste(rep("?", ncol(dataframe)), collapse = ",")
        ),
        params = as.list(dataframe) |> unname()
      )
    },
    error = function(e) {
      if (sqliteIsTransacting(conn)) {
        dbRollback(conn)
      }

      dbFinish(conn)
      stop(e)
    }
  )

  if (commit) {
    dbCommit(conn)
  }

  if (class(dbInfo) == "character") {
    dbFinish(conn)
  }

  return(result)
}
