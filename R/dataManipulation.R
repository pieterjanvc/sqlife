#' Function to add rows in a data frame to and existing database table
#'
#' @param dataframe Data frame to add to table (must contain required rows)
#' @param dbInfo A dbInfo object
#' @param table Name of the table to insert to in the database
#' @param inherit (Default = T) If an active connection is passed, continue with
#'  the current transaction
#' @param returnData (Default = T) Return a dataframe with inserted rows.
#' if FALSE, nothing is returned
#' @param constraints (Default = T) Enforce foreign key constraints
#'
#' @import RSQLite dplyr
#'
#' @returns A data frame with the data that was inserted
#'
#' Note on DB commit:
#'  - If an existing connection was inherited, the results are added to the
#'  transaction without commit
#'  - If the connection was new or inherit = F, the results are
#'  automatically committed
#' @export
#'
tbl_insert <- function(
  dataframe,
  dbInfo,
  table,
  inherit = T,
  returnData = T,
  constraints = T
) {
  if (!is.data.frame(dataframe)) {
    stop(
      "Dataframe expected but object with class ",
      paste(class(dataframe), collapse = " and "),
      " found"
    )
  }

  conn <- dbGetConn(
    dbInfo,
    inherit = inherit,
    enforceKeyConstraints = constraints
  )

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

  . <- dbFinish(conn)

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
#' @param dataframe Data frame with columns to update (must contain primary key columns)
#' @param dbInfo A dbInfo object
#' @param table Name of the table to  update in the database
#' @param inherit (Default = T) If an active connection is passed, continue with
#'  the current transaction
#' @param returnData (Default = T) Return a dataframe with updated rows.
#' if FALSE, nothing is returned
#' @param constraints (Default = T) Enforce foreign key constraints
#'
#' @import RSQLite dplyr
#'
#' @returns A data frame with the data that was updated
#'
#' Note on DB commit:
#'  - If an existing connection was inherited, the results are added to the
#'  transaction without commit
#'  - If the connection was new or inherit = F, the results are
#'  automatically committed
#' @export
#'
tbl_update <- function(
  dataframe,
  dbInfo,
  table,
  inherit = T,
  returnData = T,
  constraints = T
) {
  if (!is.data.frame(dataframe)) {
    stop(
      "Dataframe expected but object with class ",
      paste(class(dataframe), collapse = " and "),
      " found"
    )
  }

  conn <- dbGetConn(
    dbInfo,
    inherit = inherit,
    enforceKeyConstraints = constraints
  )

  check <- dbColumnCheck(dataframe, conn, table, notNUllError = F)

  if (!check$success) {
    . <- dbFinish(
      conn,
      error = paste("\n- ", paste(check$msg, collapse = "\n\n- "))
    )
  }

  # Non primary key columns to update
  toUpdate <- colnames(dataframe)[!colnames(dataframe) %in% check$pk]

  # Make sure the data frame is in correct order when converting to params
  originalOrder <- colnames(dataframe)
  dataframe <- dataframe |> select(!all_of(check$pk), all_of(check$pk))

  tryCatch(
    {
      result <- dbGetQuery(
        conn,
        sprintf(
          'UPDATE "%s" SET %s WHERE %s RETURNING "%s"',
          table,
          paste(sprintf('"%s" = ?', toUpdate), collapse = ", "),
          paste(sprintf('"%s" = ?', check$pk), collapse = " AND "),
          paste(originalOrder, collapse = '","')
        ),
        params = as.list(dataframe) |> unname()
      )
    },
    error = function(e) {
      . <- dbFinish(conn, error = e)
    }
  )

  . <- dbFinish(conn)

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
#' @param dataframe Data frame with columns to update (must contain primary key columns)
#' @param dbInfo A dbInfo object
#' @param table Name of the table to delete rows from in the database
#' @param inherit (Default = T) If an active connection is passed, continue with
#'  the current transaction
#' @param returnData (Default = T) Return a dataframe with deleted rows.
#' if FALSE, nothing is returned
#' @param constraints (Default = T) Enforce foreign key constraints
#'
#' @import RSQLite dplyr
#'
#' @returns A data frame with the data that was deleted
#'
#' Note on DB commit:
#'  - If an existing connection was inherited, the results are added to the
#'  transaction without commit
#'  - If the connection was new or inherit = F, the results are
#'  automatically committed
#' @export
#'
tbl_delete <- function(
  dataframe,
  dbInfo,
  table,
  inherit = T,
  returnData = T,
  constraints = T
) {
  if (!is.data.frame(dataframe)) {
    stop(
      "Dataframe expected but object with class ",
      paste(class(dataframe), collapse = " and "),
      " found"
    )
  }

  conn <- dbGetConn(
    dbInfo,
    inherit = inherit,
    enforceKeyConstraints = constraints
  )

  check <- dbColumnCheck(dataframe, conn, table, notNUllError = F)

  if (!check$success) {
    . <- dbFinish(
      conn,
      error = paste("\n- ", paste(check$msg, collapse = "\n\n- "))
    )
  }

  dataframe <- dataframe |> select(all_of(check$pk))

  tryCatch(
    {
      result <- dbGetQuery(
        conn,
        sprintf(
          'DELETE FROM "%s" WHERE %s RETURNING *',
          table,
          paste(sprintf('"%s" = ?', check$pk), collapse = " AND ")
        ),
        params = as.list(dataframe) |> unname()
      )
    },
    error = function(e) {
      . <- dbFinish(conn, error = e)
    }
  )

  . <- dbFinish(conn)

  if (returnData) {
    return(result)
  } else {
    invisible()
  }
}
