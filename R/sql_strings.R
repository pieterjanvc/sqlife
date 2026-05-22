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
#'  NOTE: This function might be slow and still generate errors as it was
#     generated uwing AI to help with the difficult SQLite parsing process
#'
#' @param file An SQLite file containing one or more statements (e.g. .sql)
#' @param string (Optional) If set, file is ignored and the provided string is parsed
#'
#' @returns A vector of individual SQLite statements in the same order as in the file
#' @export
#'
sql_statements <- function(file, string) {
  . <- dbplyr::sql("") # This dummy is just so dbplyr is allowed in imports

  # Set input SQL string
  if (missing(string)) {
    sql <- paste(readLines(file), collapse = "\n")
  } else {
    sql <- string
  }

  statements <- character()
  statement <- ""

  # State tracking
  in_single_quote <- FALSE
  in_double_quote <- FALSE
  in_backtick <- FALSE
  in_bracket <- FALSE
  in_line_comment <- FALSE
  in_block_comment <- FALSE

  nesting_level <- 0

  i <- 1
  n <- nchar(sql)

  while (i <= n) {
    c <- substr(sql, i, i)
    c_next <- if (i < n) substr(sql, i + 1, i + 1) else ""

    # 1. Handle Comments (High Priority)
    if (in_block_comment) {
      if (c == "*" && c_next == "/") {
        in_block_comment <- FALSE
        i <- i + 2
        next
      }
      i <- i + 1
      next
    }
    if (in_line_comment) {
      if (c == "\n") {
        in_line_comment <- FALSE
        statement <- paste0(statement, c)
      }
      i <- i + 1
      next
    }

    # 2. Check for start of comments (only if not in a string/identifier)
    if (!in_single_quote && !in_double_quote && !in_backtick && !in_bracket) {
      if (c == "-" && c_next == "-") {
        in_line_comment <- TRUE
        i <- i + 2
        next
      }
      if (c == "/" && c_next == "*") {
        in_block_comment <- TRUE
        i <- i + 2
        next
      }
    }

    # 3. Handle Quoting & Identifiers
    # Single quotes
    if (c == "'" && !in_double_quote && !in_backtick && !in_bracket) {
      if (in_single_quote && c_next == "'") {
        statement <- paste0(statement, "''")
        i <- i + 2
        next
      }
      in_single_quote <- !in_single_quote
    } else if (c == '"' && !in_single_quote && !in_backtick && !in_bracket) {
      # Double quotes
      if (in_double_quote && c_next == '"') {
        statement <- paste0(statement, '""')
        i <- i + 2
        next
      }
      in_double_quote <- !in_double_quote
    } else if (
      c == '`' && !in_single_quote && !in_double_quote && !in_bracket
    ) {
      # Backticks (SQLite/MySQL style)
      in_backtick <- !in_backtick
    } else if (
      c == '[' && !in_single_quote && !in_double_quote && !in_backtick
    ) {
      # Square Brackets (SQLite/T-SQL style)
      in_bracket <- TRUE
    } else if (c == ']' && in_bracket) {
      in_bracket <- FALSE
    }

    # 4. Handle Nesting (BEGIN/CASE ... END)
    # Only check if we are in "pure" SQL code (not strings/comments)
    if (
      !in_single_quote &&
        !in_double_quote &&
        !in_backtick &&
        !in_bracket &&
        !in_line_comment &&
        !in_block_comment
    ) {
      # Helper to check if a word is a standalone keyword
      is_keyword <- function(pos, word) {
        end_pos <- pos + nchar(word) - 1
        if (toupper(substr(sql, pos, end_pos)) != word) {
          return(FALSE)
        }
        # Ensure it's not part of another word (e.g., BEGINning)
        prev_char <- if (pos > 1) substr(sql, pos - 1, pos - 1) else " "
        next_char <- if (end_pos < n) {
          substr(sql, end_pos + 1, end_pos + 1)
        } else {
          " "
        }
        !grepl("[A-Z0-9_]", prev_char) && !grepl("[A-Z0-9_]", next_char)
      }

      if (is_keyword(i, "BEGIN") || is_keyword(i, "CASE")) {
        nesting_level <- nesting_level + 1
      } else if (is_keyword(i, "END")) {
        nesting_level <- max(0, nesting_level - 1)
      }
    }

    # 5. Semicolon Logic
    if (
      c == ";" &&
        !in_single_quote &&
        !in_double_quote &&
        !in_backtick &&
        !in_bracket &&
        nesting_level == 0
    ) {
      statements <- c(statements, trimws(statement))
      statement <- ""
      i <- i + 1
      next
    }

    statement <- paste0(statement, c)
    i <- i + 1
  }

  # Clean up last statement
  if (nchar(trimws(statement)) > 0) {
    statements <- c(statements, trimws(statement))
  }

  return(statements)
}

#' Get the SQLite schema statement for a database
#'
#' @param conn DB connection
#' @param include (Optional) If not set, all tables are included
#' @param exclude (Default = "sqlite_sequence") tables to exclude
#' @param collapse
#'
#' @returns A string or named character vectors with SQL statements
#' @export
#'
sql_statement_schema <- function(
  conn,
  include,
  exclude = c("sqlite_sequence"),
  collapse = F
) {
  tables <- dbListTables(conn)

  if (!missing(include)) {
    check <- setdiff(include, tables)
    if (length(check) > 0) {
      stop(
        "The following tables do not exist in the database: ",
        paste(check, collapse = ", ")
      )
    }
    tables <- include
  } else {
    tables <- tables[!tables %in% exclude]
  }

  # Get the table SQL creation statements
  statements <- dbGetQuery(
    conn,
    glue(
      "SELECT sql, name FROM sqlite_schema ",
      "WHERE type IN ('table', 'index', 'trigger', 'view') AND name IN (",
      paste0("'", paste0(tables, collapse = "','"), "'"),
      ");"
    )
  )

  if (collapse) {
    paste(statements$sql, collapse = ";\n\n")
  } else {
    setNames(statements$sql, statements$name)
  }
}

#' Get the SQLite data statement for a database
#'
#'   NOTE: The results can get large based on the database size as it will
#'   be in uncompressed, plain text
#'
#' @param conn DB connection
#' @param include (Optional) If not set, all tables are included
#' @param exclude (Default = "sqlite_sequence") tables to exclude
#' @param collapse
#'
#' @returns A string or named character vectors with SQL statements
#' @export
#'
sql_statement_data <- function(
  conn,
  include,
  exclude = c("sqlite_sequence"),
  collapse = F
) {
  # TODO - subset of data
  # 1 Limit the number of rows returned
  # 2 Add them into a temp database wtih PRAGMA foreign_keys = OFF;
  # 3 Run PRAGMA foreign_key_check; and delete all offending rows

  tables <- dbListTables(conn)

  if (!missing(include)) {
    check <- setdiff(include, tables)
    if (length(check) > 0) {
      stop(
        "The following tables do not exist in the database: ",
        paste(check, collapse = ", ")
      )
    }
    tables <- include
  } else {
    tables <- tables[!tables %in% exclude]
  }

  # Loop through tables and generate INSERT statements
  statements <- sapply(tables, function(tbl) {
    data <- dbReadTable(conn, tbl)

    if (nrow(data) == 0) {
      return()
    }

    values <- lapply(data, function(col) {
      if (is.numeric(col)) {
        ifelse(is.na(col), "NULL", col)
      } else {
        ifelse(is.na(col), "NULL", sprintf("'%s'", col))
      }
    })

    values <- do.call(paste, c(values, sep = ", "))
    values <- paste0(values, collapse = "),\n(")
    paste0(
      'INSERT INTO "',
      tbl,
      '" VALUES\n(',
      values,
      ');'
    )
  })

  empty <- sapply(statements, is.null)

  tables <- tables[!empty]
  statements <- statements[!empty]

  if (collapse) {
    paste(statements, collapse = ";\n\n")
  } else {
    setNames(statements, tables)
  }
}

#' Check if a table / column name is valid for SQLite
#'
#' - Must start with _ or letter
#' - Cannot contain special characters unless double quoted
#' - Cannot contain double quote as part of the name
#'
#' @param name Name to check
#'
#' @returns True or False
#' @export
#'
check_names_sql <- function(name) {
  grepl("^([a-zA-Z_][a-zA-Z0-9_]*|\"(?:[^\"]|\"\")+\")$", name)
}
