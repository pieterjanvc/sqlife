# Internal DBML spec — all keyword strings in one place.
# Update this list when the DBML spec changes.
# Parser ground truth: https://github.com/holistics/dbml/blob/master/packages/dbml-parse/src/core/parser/parser.ts
# Docs: https://dbml.dbdiagram.io/docs/
.dbml_spec <- list(
  keyword = list(
    pk = "pk",
    not_null = "not null",
    unique = "unique",
    increment = "increment",
    default = "default",
    note = "note"
  ),
  ref_op = list(
    many_to_one = ">",
    one_to_many = "<",
    one_to_one = "-",
    many_to_many = "<>"
  ),
  # Valid ON DELETE / ON UPDATE actions (lowercase as required by DBML)
  ref_action = c("cascade", "restrict", "set null", "set default", "no action"),
  index_type = c("btree", "hash"),
  database_type = "SQLite",
  indent = "  "
)

#' Format a SQLite default value as a DBML default expression
#'
#' Converts the raw default string stored in SQLite DDL into the correct DBML
#' literal form: null, boolean, numeric, single-quoted string, or backtick expression.
#'
#' @param val The raw default value string from PRAGMA table_info dflt_value column
#'
#' @returns A character string ready for use after `default:`, or NULL if val is NA/NULL
#'
dbml_format_default <- function(val) {
  if (is.na(val) || is.null(val)) {
    return(NULL)
  }
  val <- trimws(val)
  if (tolower(val) == "null") {
    return("null")
  }
  if (tolower(val) %in% c("true", "false")) {
    return(tolower(val))
  }
  if (grepl("^-?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?$", val)) {
    return(val)
  }
  if (grepl("^'.*'$", val)) {
    return(val)
  }
  paste0("`", val, "`")
}

#' Build the DBML settings string for a single column
#'
#' @param col A single-row data frame from PRAGMA table_info (columns: pk, notnull, dflt_value)
#' @param autoincrement (Default = FALSE) Whether the column uses AUTOINCREMENT
#'
#' @returns A character string of the form `[pk, not null, default: x]`, or `""` if no settings apply
#'
dbml_col_settings <- function(col, autoincrement = FALSE) {
  s <- .dbml_spec$keyword
  settings <- character(0)
  if (col$pk > 0) {
    settings <- c(settings, s$pk)
  }
  if (autoincrement) {
    settings <- c(settings, s$increment)
  }
  if (col$notnull == 1) {
    settings <- c(settings, s$not_null)
  }
  if (!is.na(col$dflt_value)) {
    fmt <- dbml_format_default(col$dflt_value)
    if (!is.null(fmt)) settings <- c(settings, paste0(s$default, ": ", fmt))
  }
  if (length(settings) == 0) {
    return("")
  }
  paste0("[", paste(settings, collapse = ", "), "]")
}

#' Render a single DBML column line
#'
#' @param col A single-row data frame from PRAGMA table_info (columns: name, type, pk, notnull, dflt_value)
#' @param autoincrement (Default = FALSE) Whether the column uses AUTOINCREMENT
#'
#' @returns A character string for one indented column line, e.g. `  "id" integer [pk, increment]`
#'
dbml_render_col <- function(col, autoincrement = FALSE) {
  ind <- .dbml_spec$indent
  type <- if (nchar(trimws(col$type)) == 0) "varchar" else col$type
  # Types containing spaces must be double-quoted per DBML spec
  if (grepl(" ", type) && !startsWith(type, '"')) {
    type <- paste0('"', type, '"')
  }
  settings <- dbml_col_settings(col, autoincrement)
  parts <- c(paste0(ind, '"', col$name, '"'), type)
  if (nchar(settings) > 0) {
    parts <- c(parts, settings)
  }
  paste(parts, collapse = " ")
}

#' Fetch index metadata for a table from SQLite PRAGMA
#'
#' Queries PRAGMA index_list and PRAGMA index_info for the given table, excluding
#' auto-generated primary key indexes (origin "pk" and sqlite_autoindex_ prefixed names).
#'
#' @param conn An open SQLiteConnection
#' @param table Table name to fetch indexes for
#'
#' @returns A list of index entries, each with elements `name`, `unique`, and `columns`
#'
dbml_fetch_indexes <- function(conn, table) {
  idx_list <- dbGetQuery(conn, sprintf('PRAGMA index_list("%s");', table))
  if (nrow(idx_list) == 0) {
    return(list())
  }
  idx_list <- idx_list[
    idx_list$origin != "pk" & !grepl("^sqlite_autoindex_", idx_list$name),
    ,
    drop = FALSE
  ]
  if (nrow(idx_list) == 0) {
    return(list())
  }
  lapply(seq_len(nrow(idx_list)), function(i) {
    idx <- idx_list[i, ]
    cols <- dbGetQuery(conn, sprintf('PRAGMA index_info("%s");', idx$name))
    list(name = idx$name, unique = as.logical(idx$unique), columns = cols$name)
  })
}

#' Render a DBML indexes block
#'
#' Builds the `indexes { }` block for a table, covering composite primary keys
#' and any named or unique indexes.
#'
#' @param indexes A list of index entries as returned by `dbml_fetch_indexes()`
#' @param composite_pk (Default = NULL) Character vector of column names forming a composite primary key
#'
#' @returns A character vector of lines for the indexes block, or NULL if there is nothing to render
#'
dbml_render_indexes <- function(indexes, composite_pk = NULL) {
  ind <- .dbml_spec$indent
  ind2 <- paste0(ind, ind)
  lines <- character(0)

  if (!is.null(composite_pk) && length(composite_pk) > 1) {
    cols <- paste0("(", paste0('"', composite_pk, '"', collapse = ", "), ")")
    lines <- c(lines, paste0(ind2, cols, " [pk]"))
  }

  for (idx in indexes) {
    col_str <- if (length(idx$columns) == 1) {
      paste0('"', idx$columns, '"')
    } else {
      paste0("(", paste0('"', idx$columns, '"', collapse = ", "), ")")
    }
    settings <- character(0)
    if (isTRUE(idx$unique)) {
      settings <- c(settings, "unique")
    }
    if (!is.null(idx$name) && nchar(idx$name) > 0) {
      settings <- c(settings, paste0('name: "', idx$name, '"'))
    }
    suffix <- if (length(settings) > 0) {
      paste0(" [", paste(settings, collapse = ", "), "]")
    } else {
      ""
    }
    lines <- c(lines, paste0(ind2, col_str, suffix))
  }

  if (length(lines) == 0) {
    return(NULL)
  }
  c(paste0(ind, "indexes {"), lines, paste0(ind, "}"))
}

#' Render a DBML Table block
#'
#' @param table Table name string
#' @param cols Data frame of columns from PRAGMA table_info for this table
#' @param autoincrement_cols Character vector of column names that use AUTOINCREMENT
#' @param indexes A list of index entries as returned by `dbml_fetch_indexes()`
#' @param composite_pk Character vector of column names forming a composite primary key, or NULL
#'
#' @returns A single character string containing the complete `Table "x" { }` block
#'
dbml_render_table <- function(
  table,
  cols,
  autoincrement_cols,
  indexes,
  composite_pk
) {
  lines <- paste0('Table "', table, '" {')
  for (i in seq_len(nrow(cols))) {
    col <- cols[i, ]
    # Composite PK columns: mark pk = 0 here; the pk goes in the indexes block instead
    if (!is.null(composite_pk) && length(composite_pk) > 1) {
      col$pk <- 0L
    }
    lines <- c(
      lines,
      dbml_render_col(col, autoincrement = col$name %in% autoincrement_cols)
    )
  }
  idx_lines <- dbml_render_indexes(indexes, composite_pk)
  if (!is.null(idx_lines)) {
    lines <- c(lines, "", idx_lines)
  }
  paste(c(lines, "}"), collapse = "\n")
}

#' Render a DBML Ref statement for a foreign key
#'
#' Produces a short-form `Ref:` line from one row of the `foreignkeyInfo` data frame
#' returned by `schemaInfo()`. ON DELETE / ON UPDATE actions are included as settings
#' when they differ from the default "no action".
#'
#' @param fk_row A single-row data frame with columns: table (child table), from (FK column),
#'   fk_table (parent table), to (referenced column), on_delete, on_update
#'
#' @returns A character string of the form `Ref: "child"."col" > "parent"."col" [delete: cascade]`
#'
dbml_render_ref <- function(fk_row) {
  ref_str <- sprintf(
    'Ref: "%s"."%s" %s "%s"."%s"',
    fk_row$table,
    fk_row$from,
    .dbml_spec$ref_op$many_to_one,
    fk_row$fk_table,
    fk_row$to
  )
  settings <- character(0)
  on_del <- tolower(trimws(fk_row$on_delete))
  on_upd <- tolower(trimws(fk_row$on_update))
  if (!is.na(on_del) && on_del != "no action") {
    settings <- c(settings, paste0("delete: ", on_del))
  }
  if (!is.na(on_upd) && on_upd != "no action") {
    settings <- c(settings, paste0("update: ", on_upd))
  }
  if (length(settings) > 0) {
    ref_str <- paste0(ref_str, " [", paste(settings, collapse = ", "), "]")
  }
  ref_str
}

#' Convert a SQLite database schema to DBML
#'
#' Generate a DBML (Database Markup Language) string from a SQLite database
#' schema, suitable for use with dbdiagram.io or other DBML-compatible tools.
#'
#' @param dbInfo A path to a SQLite database file or an existing SQLiteConnection
#' @param project_name Optional project name for the DBML Project block
#' @param note Optional note string added to the Project block#'
#' @param exclude (Default = "sqlite_sequence") Character vector of table names to exclude
#' @param include Optional character vector of table names to include#'
#'
#' @import RSQLite dplyr
#' @returns A character string containing the complete DBML schema definition
#' @export
#'
schema_dbml <- function(
  dbInfo,
  project_name,
  note,
  include,
  exclude = c("sqlite_sequence")
) {
  conn <- dbGetConnFromInfo(
    dbInfo,
    startTransaction = F,
    constraints = T,
    silentErr = T,
    busyTimeout = 0
  )
  schemainfo <- schemaInfo(conn, exclude = exclude, include = include)
  tables <- unique(schemainfo$tableInfo$table)
  blocks <- character(0)

  if (!missing(project_name)) {
    proj <- c(
      paste0('Project "', project_name, '" {'),
      paste0('  database_type: "', .dbml_spec$database_type, '"')
    )
    if (!missing(note)) {
      proj <- c(proj, paste0("  note: '", note, "'"))
    }
    blocks <- c(blocks, paste(c(proj, "}"), collapse = "\n"))
  }

  ref_lines <- character(0)

  for (tbl in tables) {
    cols <- schemainfo$tableInfo |> filter(table == tbl)

    # Detect AUTOINCREMENT from the original DDL (sqlite_master is the only reliable source)
    create_sql <- dbGetQuery(
      conn,
      "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = ?",
      params = list(tbl)
    )$sql
    autoincrement_cols <- character(0)
    if (
      length(create_sql) > 0 &&
        isTRUE(grepl("AUTOINCREMENT", create_sql, ignore.case = TRUE))
    ) {
      pk_col <- cols$name[cols$pk == 1]
      if (length(pk_col) == 1) autoincrement_cols <- pk_col
    }

    pk_cols <- cols$name[cols$pk > 0]
    composite_pk <- if (length(pk_cols) > 1) pk_cols else NULL
    indexes <- dbml_fetch_indexes(conn, tbl)
    blocks <- c(
      blocks,
      dbml_render_table(tbl, cols, autoincrement_cols, indexes, composite_pk)
    )

    has_fks <- !is.null(schemainfo$foreignkeyInfo) &&
      nrow(schemainfo$foreignkeyInfo) > 0
    if (has_fks) {
      fks <- schemainfo$foreignkeyInfo |> filter(table == tbl)
      for (i in seq_len(nrow(fks))) {
        ref_lines <- c(ref_lines, dbml_render_ref(fks[i, ]))
      }
    }
  }

  if (length(ref_lines) > 0) {
    blocks <- c(blocks, paste(ref_lines, collapse = "\n"))
  }
  dbFinishFromInfo(conn, commit = F, showWarning = F)
  paste(blocks, collapse = "\n\n")
}
