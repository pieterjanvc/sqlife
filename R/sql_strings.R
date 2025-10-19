#' Check if strings quoted
#'
#' @param x strings to check
#'
#' @importFrom stringr str_detect
#'
#' @returns Data frame with annotation
#' @export
#'
sql_quoteCheck <- function(x) {
  data.frame(
    x = x,
    singleQuoted = str_detect(x, "^'.*\'$"),
    doubleQuoted = str_detect(x, "^\".*\"$")
  )
}


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
sql_create_basic <- function(dataframe, tableName, showOutput = T) {
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


#' Build a CREATE statement from tableInfo data frame
#'
#' @param tableInfo A tableInfo dataframe
#' @param tableName Name for the new table
#' @param autoQuote (Default = T) Quote TEXT defautl values. Set to FALSE
#' in case you are providing functions or text is already quoted
#'
#' @returns A list with the statement and status info
#' @export
#'
sql_create <- function(tableInfo, tableName, autoQuote = T) {
  info <- tableInfo
  statusCodes <- c()
  msg <- c()

  # Check PK
  if (sum(info$pk) == 0) {
    statusCodes <- c(statusCodes, -1)
    msg <- c(msg, "There is no primary key defined")
  }

  # Check PK
  if (any(info$pk & info$default != "")) {
    statusCodes <- c(statusCodes, -2)
    msg <- c(msg, "Primary keys cannot have a default value")
  }

  # Check NOT NULL
  if (any(info$pk & info$nn)) {
    statusCodes <- c(statusCodes, 2)
    msg <- c(msg, "NOT NULL is ignored for primary keys as this is implied")
  }

  # Check ODC without FK
  if (any(info$odc & info$fkid == "")) {
    statusCodes <- c(statusCodes, -3)
    msg <- c(msg, "ON DELETE CASCASE can only be set for foreign keys")
  }

  # Check FK for PK
  if (any(info$pk & info$fkid != "")) {
    statusCodes <- c(statusCodes, -4)
    msg <- c(msg, "Primary keys cannot be foreign keys at the same time")
  }

  # Check default values
  info$validDefault = mapply(
    function(val, type) {
      pat <- case_when(
        type == "INTEGER" ~ "^\\d+$",
        type == "REAL" ~ "^\\d*(\\.\\d+)?$",
        TRUE ~ ".*"
      )

      val == "" || str_detect(val, pat)
    },
    val = info$default,
    type = info$type
  )

  if (!all(info$validDefault)) {
    statusCodes <- c(statusCodes, -5)
    msg <- c(
      msg,
      paste(
        "The following attributes / columns do no have a correct default value type:",
        paste(info$name[!info$validDefault], collapse = ", ")
      )
    )
  }

  # Check for missing attribute names
  check <- is.na(info$name) | info$name == ""
  if (any(check)) {
    statusCodes <- c(statusCodes, -6)
    msg <- c(
      msg,
      paste(
        "The following attributes / columns have no name:",
        paste(info$name[check], collapse = ", ")
      )
    )
    info$name[check] <- "<undefined>"
  }

  compoundKey <- sum(info$pk) > 1

  # Check if we need to quote
  x <- ifelse(info$type == "TEXT" & autoQuote, "'", "")

  statement <- sprintf(
    '"%s" %s%s%s',
    info$name,
    info$type,
    case_when(
      info$pk & !compoundKey ~ " PRIMARY KEY",
      info$nn & !info$pk ~ " NOT NULL",
      TRUE ~ ""
    ),
    ifelse(
      info$default != "",
      sprintf(" DEFAULT(%s%s%s)", x, info$default, x),
      ""
    )
  )

  # Compound primary key
  if (compoundKey) {
    statement <- c(
      statement,
      sprintf(
        'PRIMARY KEY("%s")',
        paste(info$name[info$pk], collapse = '", "')
      )
    )
  }

  # Foreign Keys
  if (any(info$fkid != "")) {
    fks <- info |> filter(fkid != "")
    statement <- c(
      statement,
      sprintf(
        'FOREIGN KEY("%s") REFERENCES "%s"("%s")%s',
        fks$name,
        fks$fktable,
        fks$fkid,
        ifelse(fks$odc, " ON DELETE CASCADE", "")
      )
    )
  }

  if (
    is.null(tableName) || is.na(tableName) || str_detect(tableName, "^\\s*$")
  ) {
    statusCodes <- c(statusCodes, -7)
    msg <- c(msg, "No table name defined")
    tableName <- "<undefined>"
  }

  statement <- sprintf(
    'CREATE TABLE "%s"(\n %s\n);',
    tableName,
    statement |> paste(collapse = ",\n ")
  )

  if (length(statusCodes) == 0) {
    statusCodes <- 1
    msg = "Statement created without issues"
  }

  return(list(
    success = all(statusCodes > 0),
    statement = statement,
    statusCodes = statusCodes,
    msg = msg
  ))
}
