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
  if (!is.data.frame(dataframe)) {
    stop(
      "Dataframe expected but object with class ",
      paste(class(dataframe), collapse = " and "),
      " found"
    )
  }

  conn <- dbGetConn(
    dbInfo,
    enforceKeyConstraints = constraints,
    startTransaction = T
  )

  if (!attr(conn, "existing") & !commit) {
    e <- paste(
      "Only existing connections can have commit = F",
      "Use dbGetConn() to open a connection first"
    )
    . <- dbFinish(conn, error = e)
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
      . <- dbFinish(conn, error = e)
    }
  )

  . <- dbFinish(conn, commit = commit)

  return(result)
}

#' Function to update a database table from rows in a dataframe
#'
#' The dataframe must contain all columns that make up the primary key and
#' can only contain columns that need to be updated
#'
#' @param dataframe Data frame with columns to update (must contain primary key columns)
#' @param dbInfo A dbInfo object
#' @param table Name of the table to  update in the database
#' @param commit (Default = T) Commit the data after update
#' @param constraints (Default = T) Enforce foreign key constraints
#'
#' @import RSQLite dplyr
#'
#' @returns A data frame with the data that was effectively inserted.
#' This will include any columns with auto generated values
#' @export
#'
tbl_update <- function(dataframe, dbInfo, table, commit = T, constraints = T) {
  if (!is.data.frame(dataframe)) {
    stop(
      "Dataframe expected but object with class ",
      paste(class(dataframe), collapse = " and "),
      " found"
    )
  }
  conn <- dbGetConn(
    dbInfo,
    enforceKeyConstraints = constraints,
    startTransaction = T
  )

  if (!attr(conn, "existing") & !commit) {
    e <- paste(
      "Only existing connections can have commit = F",
      "Use dbGetConn() to open a connection first"
    )
    . <- dbFinish(conn, error = e)
  }

  # Get info about the columns in the table of interest
  info <- dbGetQuery(conn, sprintf('PRAGMA table_info("%s");', table))

  if (nrow(info) == 0) {
    . <- dbFinish(
      conn,
      error = paste("The table", table, "is not found in the database")
    )
  }

  pks <- info$name[info$pk]
  check <- pks %in% colnames(dataframe)

  if (!all(check)) {
    . <- dbFinish(
      conn,
      error = paste0(
        "The primary key columns '",
        paste(pks, collapse = "', '"),
        "' must be in the dataframe. Use custom queries for advanced updates"
      )
    )
  }

  check <- colnames(dataframe) %in% info$name

  if (!all(check)) {
    . <- dbFinish(
      conn,
      error = paste0(
        "The following colums are not in the '",
        table,
        "' table: '",
        paste(colnames(dataframe)[!check], collapse = "', '"),
        "'"
      )
    )
  }

  check <- dataframe |> select(all_of(pks))
  if (nrow(check) != nrow(distinct(check))) {
    . <- dbFinish(
      conn,
      error = paste(
        "There can only be one row per primary key"
      )
    )
  }

  # Non primary key columns to update
  toUpdate <- colnames(dataframe)[!colnames(dataframe) %in% pks]

  # Make sure the data frame is in correct order when converting to params
  originalOrder <- colnames(dataframe)
  dataframe <- dataframe |> select(!all_of(pks), all_of(pks))

  tryCatch(
    {
      result <- dbGetQuery(
        conn,
        sprintf(
          'UPDATE "%s" SET %s WHERE %s RETURNING "%s"',
          table,
          paste(sprintf('"%s" = ?', toUpdate), collapse = " AND "),
          paste(sprintf('"%s" = ?', pks), collapse = " AND "),
          paste(originalOrder, collapse = '","')
        ),
        params = as.list(dataframe) |> unname()
      )
    },
    error = function(e) {
      . <- dbFinish(conn, error = e)
    }
  )

  . <- dbFinish(conn, commit = commit)

  return(result)
}
