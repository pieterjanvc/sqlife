#' Set a table attribute / column
#'
#' @param conn Database connection
#' @param table Name of the table to change
#' @param attr Attribute / column name
#' @param type Any of INTEGER, TEXT, REAL or BLOB
#' @param pk (Optional) If set to T, primary key flag
#' @param notNull (Optional) If set to T, NOT NULL enforced. Will happen
#' automatically for primary keys, so no need to set it
#' @param default (Optional) Default value
#'
#' @returns The database connection
#' @export
#'
schema_modify <- function(conn, table, attr, type, pk, notnull, default) {
  ## TODO
  # UNIQUE and AUTOINCREMENT are in the sqlite_sequence, and index tables
  # respectively and are not returned with basic SQL info
  # conn <- dbNewFromSchema(schema = "inst/example.sql", memory = ":memory:")$conn

  stop("This functions has not been implemented yet")

  # Check inputs
  check <- c(table, attr)
  check <- check[!sql_check_name(check)]

  if (length(check) > 0) {
    stop(
      "The following names are not valid for an SQL table or attribute / column name: ",
      paste(unique(check), collapse = ", ")
    )
  }

  check <- data.frame(table = table, attr = attr) |>
    group_by(table, attr) |>
    filter(n() > 1) |>
    ungroup()

  if (nrow(check) > 0) {
    stop(
      "The following attributes / columns do not have a unique names ",
      "in their respective tables\n\n",
      dfAsText(check |> distinct())
    )
  }

  type <- toupper(type)
  check <- type[!type %in% c("INTEGER", "TEXT", "REAL", "BLOB")]

  if (length(check) > 0) {
    stop("The type of '", unique(type), "' is not a valid SQLite data type")
  }

  if (missing(pk)) {
    pk <- F
  } else if (!is.logical(pk)) {
    stop("PK error")
  }

  if (missing(notnull)) {
    notnull <- F
  } else if (!is.logical(notnull)) {
    stop("NOT NULL error")
  } else if (pk & !notnull) {
    warning("NOT NULL is ignored for primary keys")
  }

  if (missing(default)) {
    default <- NA_character_
  } else if (is.atomic(default)) {
    default <- ifelse(
      is.numeric(default),
      as.character(default),
      sprintf("'%s'", default)
    )
  } else {
    stop("Default value error")
  }

  # Combine all changes into a data frame
  changes <- data.frame(
    table = table,
    name = attr,
    type = type,
    pk = pk,
    notnull = notnull,
    default = default
  )

  # Get the current schema info
  schemainfo <- schemaInfo(conn)

  # New tables
  # ALTER TABLE table_name
  # RENAME COLUMN old_column_name TO new_column_name;

  return(conn)
}

#' Rename an existing table
#'
#'  NOTE: Foreign keys will automatically update where needed
#'
#' @param conn Database connection
#' @param table Name of the existing table
#' @param newName New table name
#' @param commit (Default = FALSE) commit change.
#'
#' @returns Database connection
#' @export
#'
schema_rename_table <- function(conn, table, newName, commit = F) {
  check_names_table(conn, existing = table, new = newName, error = T)

  # Start transaction if needed
  if (!commit & !sqliteIsTransacting(conn)) {
    dbBegin(conn)
  }

  # Alter the table names
  statements <- paste("ALTER TABLE", table, "RENAME TO", newName, ";")

  for (statement in statements) {
    q <- dbExecute(conn, statement)
  }

  return(conn)
}

#' Rename an existing attribute / column
#'
#'  NOTE: Foreign keys will automatically update where needed
#'
#' @param conn Database connection
#' @param table Name of the table where to change the attribute / column
#' @param attr Name of the existing attribute
#' @param newName New attribute name
#' @param commit (Default = FALSE) commit change.
#'
#' @returns Database connection
#' @export
#'
schema_rename_attr <- function(conn, table, attr, newName, commit = F) {
  # TODO
  stop("This functions has not been implemented yet")

  return(conn)
}


#' Check new / existing tables names
#'
#' @param conn 	Database connection
#' @param existing List of table names expecting to already exist
#' @param new List of table names to add or alter
#' @param error (Default = F) If TRUE, throw an error instead of returning the
#' result of the check
#'
#' @returns List with success bool, status codes, messages and pre-formatted
#' combined message. If error is TRUE, error or no return
#'
#' @export
#'
check_names_table <- function(conn, existing = NULL, new = NULL, error = F) {
  # Setup
  tables <- dbListTables(conn)
  statusCode <- c()
  msg <- c()

  # EXISTING
  existing <- unique(existing)
  check <- existing[!existing %in% tables]

  if (length(check) > 0) {
    statusCode <- c(statusCode, -1)
    msg <- c(
      msg,
      paste0(
        "The following tables do not exist: ",
        paste(check, collapse = ", ")
      )
    )
  }

  # NEW
  new <- unique(new)
  check <- new[!check_names_sql(new)]

  if (length(check) > 0) {
    statusCode <- c(statusCode, -2)
    msg <- c(
      msg,
      paste0(
        "The following new names are not valid for an SQL table: ",
        paste(check, collapse = ", ")
      )
    )
  }

  check <- new[new %in% tables]

  if (length(check) > 0) {
    statusCode <- c(statusCode, -3)
    msg <- c(
      msg,
      paste0(
        "The following tables already exist: ",
        paste(check, collapse = ", ")
      )
    )
  }

  # RESULT
  if (length(statusCode) == 0) {
    statusCode <- 1
    msg <- "No issues with provided table names"
    preFormatted = msg
  } else {
    preFormatted <- paste0(
      "TABLE NAMING ISSUES\n- ",
      paste0(msg, collapse = "\n- ")
    )
  }
  success <- all(statusCode > 0)

  # If stop on error
  if (error) {
    if (success) {
      return(invisible(T))
    } else {
      stop(preFormatted)
    }
  }

  return(list(
    success = success,
    statusCode = statusCode,
    msg = msg,
    preFormatted = preFormatted
  ))
}

#' Check new / existing tables names
#'
#' @param conn Database connection
#' @param toCheck Data frame with 3 columns
#' - table: table name to check
#' - attr: attribute to check
#' - new: Check for new of existing (if attr is NA, table name is checked)
#' @param error (Default = F) If TRUE, throw an error instead of returning the
#' result of the check
#'
#' @returns List with success bool, status codes, messages and pre-formatted
#' combined message. If error is TRUE, error or no return
#'
#' @export
check_names_attr <- function(conn, toCheck, error = F) {
  # Check input data frame
  check <- setdiff(c("table", "attr", "new"), colnames(toCheck))
  if (length(check) > 0 | !all(complete.cases(toCheck))) {
    msg <- paste(
      "The argument toCheck must be a complete data frame with columns:",
      "table (char), attr (char) and new (bool)."
    )

    if (error) {
      stop(msg)
    }

    return(list(
      success = F,
      statusCode = -1,
      data = NULL,
      preFormatted = msg
    ))
  }

  # Check if all tables exist
  check <- check_names_table(conn, existing = toCheck$table, error = F)

  if (!check$success & error) {
    stop(check$preformatted)
  } else if (!check$success) {
    return(list(
      success = F,
      statusCode = -2,
      data = NULL,
      preFormatted = check$preformatted
    ))
  }

  # Check the attributes against the ones in the DB
  tableInfo <- schemaInfo(conn, include = toCheck |> pull(table))$tableInfo

  toCheck <- toCheck |>
    left_join(
      tableInfo |> select(table, attr = name, found = pk),
      by = c("table", "attr")
    ) |>
    mutate(
      valid = check_names_sql(attr),
      found = ifelse(is.na(found), F, T),
      pass = !(new & found) & valid
    )

  # Results
  success <- all(toCheck$pass)
  preFormatted <- paste0(
    "Attribute issues detected\n\n",
    dfAsText(toCheck |> filter(!pass))
  )

  if (error & !success) {
    stop(preFormatted)
  } else if (!success) {
    return(list(
      success = F,
      statusCode = -1,
      data = toCheck,
      preFormatted = preFormatted
    ))
  } else {
    return(list(
      success = T,
      statusCode = 1,
      data = NULL,
      preFormatted = "No attribute issues detected"
    ))
  }
}
