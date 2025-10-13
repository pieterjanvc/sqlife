#' Function to add rows in a data frame to and existing database table
#'
#' @param dataframe Data frame to add to table (must contain required rows)
#' @param dbInfo A dbInfo object
#' @param table Name of the table to insert to in the database
#' @param commit (Default = T) Commit the data after insertion
#' @param constraints (Default = T) Enforce foreign key constraints
#'
#' @import RSQLite
#'
#' @returns A data frame with the data that was effectively inserted.
#' This will include any columns with auto generated values
#' @export
#'
tbl_insert <- function(dataframe, dbInfo, table, commit = T, constraints = T) {
  if (class(dbInfo) == "character" & !commit) {
    stop(
      "Only existing connections can have commit = F",
      "Use dbGetConn() to open a connection first"
    )
  }

  conn <- dbGetConn(dbInfo)

  if (constraints) {
    result <- dbExecute(conn, "PRAGMA foreign_keys = ON")
  }

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
