#' Create an SQL CREATE TABLE statement from a data frame
#'
#' This can be used as the bases for creating an SQLite file where you can then
#' manually add other table info (e.g. keys and constraints)
#'
#' @param dataframe Data frame to create statement from
#' @param tableName Name for the new table in the database
#' @param showOutput (Default = TRUE). Show statement in console
#'
#' @import RSQLite
#'
#' @returns String with SQLite statement
#' @export
#'
sql_create <- function(dataframe, tableName, showOutput = T) {
  # Write the dataframe to an in-memory SQLite DB
  conn <- dbConnect(RSQLite::SQLite(), ":memory:")
  x <- dbWriteTable(conn, tableName, dataframe)
  # Extract the SQL statement
  info <- dbGetQuery(
    conn,
    sprintf(
      "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = '%s';",
      tableName
    )
  )
  dbDisconnect(conn)
  # Update the quotes used
  info <- gsub("`", "\"", as.character(info))
  info <- paste0(info, ";")

  #Show output if set
  if (showOutput) {
    cat(info)
  }

  return(invisible(info))
}

#' Extract all statements from an SQLite file
#'
#' @param file An SQLite file (e.g. .sql)
#'
#' @returns A vector of individual SQLite statements in the same order as in the file
#' @export
#'
sql_statements <- function(file) {
  . <- dbplyr::sql("") # This dummy is just so dbplyr is allowed in imports
  # Read file as one string
  sql <- paste(readLines(file), collapse = "\n")

  statements <- character()
  statement <- ""

  in_single_quote <- FALSE
  in_double_quote <- FALSE
  in_line_comment <- FALSE
  in_block_comment <- FALSE

  i <- 1
  n <- nchar(sql)

  while (i <= n) {
    c <- substr(sql, i, i)
    c_next <- if (i < n) substr(sql, i + 1, i + 1) else ""

    # Inside block comment, look for end */
    if (in_block_comment) {
      if (c == "*" && c_next == "/") {
        in_block_comment <- FALSE
        i <- i + 2
        next
      } else {
        i <- i + 1
        next
      }
    }

    # Inside line comment, skip till newline
    if (in_line_comment) {
      if (c == "\n") {
        in_line_comment <- FALSE
        statement <- paste0(statement, c) # keep newline to separate statements
      }
      i <- i + 1
      next
    }

    # If not in comment, check quotes and start comments
    if (!in_single_quote && !in_double_quote) {
      # Detect start of line comment --
      if (c == "-" && c_next == "-") {
        in_line_comment <- TRUE
        i <- i + 2
        next
      }

      # Detect start of block comment /*
      if (c == "/" && c_next == "*") {
        in_block_comment <- TRUE
        i <- i + 2
        next
      }
    }

    # Handle string quotes (respect escapes)
    if (c == "'" && !in_double_quote) {
      # Handle escaped single quotes ''
      if (in_single_quote && c_next == "'") {
        statement <- paste0(statement, "''")
        i <- i + 2
        next
      }
      in_single_quote <- !in_single_quote
      statement <- paste0(statement, c)
      i <- i + 1
      next
    }

    if (c == '"' && !in_single_quote) {
      # Handle escaped double quotes ""
      if (in_double_quote && c_next == '"') {
        statement <- paste0(statement, '""')
        i <- i + 2
        next
      }
      in_double_quote <- !in_double_quote
      statement <- paste0(statement, c)
      i <- i + 1
      next
    }

    # End of statement
    if (c == ";" && !in_single_quote && !in_double_quote) {
      statements <- c(statements, trimws(statement))
      statement <- ""
      i <- i + 1
      next
    }

    # Normal character, append
    statement <- paste0(statement, c)
    i <- i + 1
  }

  # Add last statement if any
  if (nchar(trimws(statement)) > 0) {
    statements <- c(statements, trimws(statement))
  }

  return(statements)
}
