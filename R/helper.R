#' Check the columns of a dataframe against a table in a database
#'
#' @param dataframe Data frame columns to check against the DB table
#' @param dbInfo dbInfo object
#' @param table Table in the database to check against
#' @param notNUllError (Default = T) Missing columns in dataframe with NOT NULL
#' in database will throw an error.
#'
#' @returns A list with details of the checks
#' @export
#'
dbColumnCheck <- function(dataframe, dbInfo, table, notNUllError = T) {
  statusCode <- integer(0)
  msg <- character(0)
  conn <- dbGetConnFromInfo(dbInfo, startTransaction = F)

  # Get info about the columns in the table of interest
  info <- dbGetQuery(conn, sprintf('PRAGMA table_info("%s");', table))

  # Table not found
  if (nrow(info) == 0) {
    dbDisconnect(conn)

    return(list(
      success = F,
      statusCode = -1,
      msg = paste("table", table, "not found")
    ))
  }

  # Primary key(s) missing
  pk <- info$name[info$pk]
  missingPK <- setdiff(pk, colnames(dataframe))

  if (length(missingPK) > 0) {
    statusCode <- c(statusCode, -2)
    msg <- c(
      msg,
      paste0(
        "The primary key columns '",
        paste(missingPK, collapse = "', '"),
        "' are not found in the dataframe."
      )
    )
  }

  # Primary keys not unique
  check <- dataframe |> select(any_of(pk))
  if (nrow(check) != nrow(distinct(check))) {
    statusCode <- c(statusCode, -3)
    msg <- c(msg, paste0("There can only be one row per primary key"))
    uniqueRows = F
  } else {
    uniqueRows = T
  }

  # Unknown columns
  notInTable <- setdiff(colnames(dataframe), info$name)

  if (length(notInTable) > 0) {
    statusCode <- c(statusCode, -4)
    msg <- c(
      msg,
      paste(
        "The following columns in the data frame are not in the '",
        table,
        "' table: '",
        paste(notInTable, collapse = "', '"),
        "'"
      )
    )
    notInTable <- notInTable
  } else {
    notInTable <- NULL
  }

  # NOT NULL columns missing - not an error in case of delete or update
  missingNotNull <- setdiff(
    info$name[info$notnull == 1 & info$pk == 0],
    colnames(dataframe)
  )

  if (length(missingNotNull) > 0) {
    statusCode <- c(statusCode, ifelse(notNUllError, -5, 2))
    msg <- c(
      msg,
      paste(
        "The following NOT NULL columns in table '",
        table,
        "' are not in the dataframe:",
        paste(missingNotNull, collapse = "', '"),
        "'"
      )
    )
    missingNotNull = missingNotNull
  } else {
    missingNotNull = NULL
  }

  dbFinishFromInfo(conn, commit = F)

  if (length(statusCode) == 0) {
    statusCode <- 1
  }

  return(list(
    success = !any(statusCode < 0),
    statusCodes = statusCode,
    msg = msg,
    pk = pk,
    missingPK = missingPK,
    uniqueRows = uniqueRows,
    notInTable = notInTable,
    missingNotNull = missingNotNull
  ))
}

#' Get the ID of an environment
#'
#' @param env (Default = current environment). Environment to get ID for
#'
#' @returns An ID in string format
#'
envID <- function(env = parent.frame()) {
  sub("^<environment: (.*)>$", "\\1", format(env))
}

#' Get the "file:line" location of a call
#'
#' Only available when the calling code was parsed with `keep.source = TRUE`
#' (the default in interactive sessions), which is not the case for most
#' installed packages running non-interactively.
#'
#' @param call An unevaluated call, e.g. from `sys.call()`
#'
#' @returns A "file:line" string, or NULL if no source reference is attached
#'
#' @importFrom utils getSrcFilename getSrcLocation
#'
callLocation <- function(call) {
  srcref <- attr(call, "srcref")

  if (is.null(srcref)) {
    return(NULL)
  }

  paste0(getSrcFilename(srcref), ":", getSrcLocation(srcref, "line"))
}

#' Generate a text version of a data frame for printing
#'
#' @param dataframe Dataframe to convert
#' @param noTibble (Default = T). Convert tibble into regular DF for simpler
#' parsing
#' @param row.names (Default = T). Display row names
#'
#' @returns string
#'
dfAsText <- function(dataframe, noTibble = T, row.names = F) {
  if (noTibble) {
    dataframe <- as.data.frame(dataframe)
  }

  paste(capture.output(print(dataframe, row.names = F)), collapse = "\n")
}
