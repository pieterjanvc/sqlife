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

#' Check if words might cause issues when used in SQLite
#'
#' @param words A vector or words to check
#'
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom janitor make_clean_names
#'
#' @returns A data frame with checks for each word
#' @export
#'
sql_wordCheck <- function(words) {
  # List of reserved keywords
  keywords = c(
    c("ACTION", "ADD", "AFTER", "ALL", "ALTER"),
    c("ALWAYS", "ANALYZE", "AND", "AS", "ASC", "ATTACH"),
    c("AUTOINCREMENT", "BEFORE", "BEGIN", "BETWEEN", "BY", "CASCADE", "CASE"),
    c("CAST", "CHECK", "COLLATE", "COLUMN", "COMMIT", "CONFLICT"),
    c("CONSTRAINT", "CREATE", "CROSS", "CURRENT", "CURRENT_DATE"),
    c("CURRENT_TIME", "CURRENT_TIMESTAMP", "DATABASE", "DEFAULT", "DEFERRABLE"),
    c("DEFERRED", "DELETE", "DESC", "DETACH", "DISTINCT", "DO"),
    c("DROP", "EACH", "ELSE", "END", "ESCAPE", "EXCEPT", "EXCLUDE"),
    c("EXCLUSIVE", "EXISTS", "EXPLAIN", "FAIL", "FILTER", "FIRST"),
    c("FOLLOWING", "FOR", "FOREIGN", "FROM", "FULL", "GENERATED", "GLOB"),
    c("GROUP", "GROUPS", "HAVING", "IF", "IGNORE", "IMMEDIATE", "IN"),
    c("INDEX", "INDEXED", "INITIALLY", "INNER", "INSERT", "INSTEAD"),
    c("INTERSECT", "INTO", "IS", "ISNULL", "JOIN", "KEY", "LAST"),
    c("LEFT", "LIKE", "LIMIT", "MATCH", "MATERIALIZED", "NATURAL", "NO"),
    c("NOT", "NOTHING", "NOTNULL", "NULL", "NULLS", "OF", "OFFSET"),
    c("ON", "OR", "ORDER", "OTHERS", "OUTER", "OVER", "PARTITION"),
    c("PLAN", "PRAGMA", "PRECEDING", "PRIMARY", "QUERY", "RAISE"),
    c("RANGE", "RECURSIVE", "REFERENCES", "REGEXP", "REINDEX", "RELEASE"),
    c("RENAME", "REPLACE", "RESTRICT", "RETURNING", "RIGHT"),
    c("ROLLBACK", "ROW", "ROWS", "SAVEPOINT", "SELECT", "SET", "TABLE", "TEMP"),
    c("TEMPORARY", "THEN", "TIES", "TO", "TRANSACTION", "TRIGGER"),
    c("UNBOUNDED", "UNION", "UNIQUE", "UPDATE", "USING", "VACUUM"),
    c("VALUES", "VIEW", "VIRTUAL", "WHEN", "WHERE", "WINDOW", "WITH", "WITHOUT")
  )

  reserved <- toupper(words) %in% keywords
  needsQuotes <- !str_detect(words, "^[A-Za-z_]+\\w*$") | reserved

  data.frame(
    word = words,
    reserved = reserved,
    needsQuotes = needsQuotes,
    lower = tolower(words),
    suggested = make_clean_names(words)
  ) |>
    group_by(lower) |>
    mutate(unique = n() == 1) |>
    ungroup() |>
    select(word, reserved, needsQuotes, unique, suggested)
}
