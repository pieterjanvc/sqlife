#' Function to add rows in a data frame to and existing database table
#'
#' @param dataframe Data frame to add to table (must contain required rows)
#' @param dbInfo Either an existing SQLite connection or a path to a database
#' @param table Name of the table to insert into in the database
#' @param commit (Default = TRUE) Commit if a connection is passed. When a path
#' is provided commit must be TRUE or will return an error
#' @param returnData (Default = TRUE) Return a dataframe with inserted rows.
#' if FALSE, nothing is returned
#' @param constraints (Default = TRUE) Ignored when connection is passed.
#' Enforce foreign key constraints
#' @param busyTimeout (Default = 0) Ignored when connection is passed. Set the
#' timeout in milliseconds when a new connection is created from a path. This
#' is only needed if concurrency is anticipated
#'
#' @import RSQLite dplyr
#'
#' @returns A data frame with the data that was inserted
#'
#' @export
#'
tbl_insert <- function(
  dataframe,
  dbInfo,
  table,
  commit = T,
  returnData = T,
  constraints = T,
  busyTimeout = 0
) {
  if (!is.data.frame(dataframe)) {
    stop(
      "Dataframe expected but object with class ",
      paste(class(dataframe), collapse = " and "),
      " found"
    )
  }

  conn <- dbGetConnFromInfo(
    dbInfo,
    startTransaction = !commit,
    constraints,
    busyTimeout,
    silentErr = T
  )
  query <- sprintf(
    'INSERT INTO "%s"("%s") VALUES(%s)%s',
    table,
    paste(colnames(dataframe), collapse = '","'),
    paste(rep("?", ncol(dataframe)), collapse = ","),
    ifelse(returnData, " RETURNING *", "")
  )
  params <- as.list(dataframe) |> unname()

  if (returnData) {
    result <- dbGetQuery(conn, query, params = params)
  } else {
    result <- dbSendQuery(conn, query, params = params)
  }

  dbFinishFromInfo(conn, commit = commit)

  if (returnData) {
    return(result)
  } else {
    invisible()
  }
}

#' Function to update a database table from rows in a dataframe
#'
#' The dataframe must contain all columns that make up the primary key and
#' can only contain columns that need to be updated
#'
#' @param dataframe Data frame to add to table (must contain required rows)
#' @param dbInfo Either an existing SQLite connection or a path to a database
#' @param table Name of the table to update in the database
#' @param commit (Default = TRUE) Commit if a connection is passed. When a path
#' is provided commit must be TRUE or will return an error
#' @param returnData (Default = TRUE) Return a dataframe with inserted rows.
#' if FALSE, nothing is returned
#' @param constraints (Default = TRUE) Ignored when connection is passed.
#' Enforce foreign key constraints
#' @param busyTimeout (Default = 0) Ignored when connection is passed. Set the
#' timeout in milliseconds when a new connection is created from a path. This
#' is only needed if concurrency is anticipated
#'
#' @import RSQLite dplyr
#'
#' @returns A data frame with the data that was inserted
#'
#' @export
#'
tbl_update <- function(
  dataframe,
  dbInfo,
  table,
  commit = T,
  returnData = T,
  constraints = T,
  busyTimeout = 0
) {
  if (!is.data.frame(dataframe)) {
    stop(
      "Dataframe expected but object with class ",
      paste(class(dataframe), collapse = " and "),
      " found"
    )
  }

  conn <- dbGetConnFromInfo(
    dbInfo,
    startTransaction = !commit,
    constraints,
    busyTimeout,
    silentErr = T
  )

  check <- dbColumnCheck(dataframe, conn, table, notNUllError = F)

  if (!check$success) {
    stop(check$msg)
  }

  # Non primary key columns to update
  toUpdate <- colnames(dataframe)[!colnames(dataframe) %in% check$pk]

  # Make sure the data frame is in correct order when converting to params
  originalOrder <- colnames(dataframe)
  dataframe <- dataframe |> select(!all_of(check$pk), all_of(check$pk))

  query <- sprintf(
    'UPDATE "%s" SET %s WHERE %s%s',
    table,
    paste(sprintf('"%s" = ?', toUpdate), collapse = ", "),
    paste(sprintf('"%s" = ?', check$pk), collapse = " AND "),
    ifelse(
      returnData,
      sprintf(" RETURNING \"%s\"", paste(originalOrder, collapse = '","')),
      ""
    )
  )
  params = as.list(dataframe) |> unname()

  if (returnData) {
    result <- dbGetQuery(conn, query, params = params)
  } else {
    result <- dbSendQuery(conn, query, params = params)
  }

  dbFinishFromInfo(conn, commit = commit)

  if (returnData) {
    return(result)
  } else {
    invisible()
  }
}

#' Function to delete rows in a database table using rows in a dataframe
#'
#' The dataframe must contain all columns that make up the primary key.
#'
#' @param dataframe Data frame to add to table (must contain required rows)
#' @param dbInfo Either an existing SQLite connection or a path to a database
#' @param table Name of the table to delete records from in the database
#' @param commit (Default = TRUE) Commit if a connection is passed. When a path
#' is provided commit must be TRUE or will return an error
#' @param returnData (Default = TRUE) Return a dataframe with inserted rows.
#' if FALSE, nothing is returned
#' @param constraints (Default = TRUE) Ignored when connection is passed.
#' Enforce foreign key constraints
#' @param busyTimeout (Default = 0) Ignored when connection is passed. Set the
#' timeout in milliseconds when a new connection is created from a path. This
#' is only needed if concurrency is anticipated
#'
#' @import RSQLite dplyr
#'
#' @returns A data frame with the data that was inserted
#'
#' @export
#'
tbl_delete <- function(
  dataframe,
  dbInfo,
  table,
  commit = T,
  returnData = T,
  constraints = T,
  busyTimeout = 0
) {
  if (!is.data.frame(dataframe)) {
    stop(
      "Dataframe expected but object with class ",
      paste(class(dataframe), collapse = " and "),
      " found"
    )
  }

  conn <- dbGetConnFromInfo(
    dbInfo,
    startTransaction = !commit,
    constraints,
    busyTimeout,
    silentErr = T
  )
  check <- dbColumnCheck(dataframe, conn, table, notNUllError = F)

  if (!check$success) {
    stop(check$msg)
  }

  dataframe <- dataframe |> select(all_of(check$pk))

  query <- sprintf(
    'DELETE FROM "%s" WHERE %s%s',
    table,
    paste(sprintf('"%s" = ?', check$pk), collapse = " AND "),
    ifelse(returnData, " RETURNING *", "")
  )
  params = as.list(dataframe) |> unname()

  if (returnData) {
    result <- dbGetQuery(conn, query, params = params)
  } else {
    result <- dbSendQuery(conn, query, params = params)
  }

  dbFinishFromInfo(conn, commit = commit)

  if (returnData) {
    return(result)
  } else {
    invisible()
  }
}
