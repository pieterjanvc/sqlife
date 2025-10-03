#' Setup a (new) SQLite database
#'
#' @param path Path to a database. If DB does not exist it will be created from the schema
#' @param schema An SQLite database schema (.sql file)
#' @param validateSchema (Default, FALSE) Check the schema of an existing database against the reference
#' @param showWarning (Default, TRUE) Show schema mismatches as a warning as well
#' @param createNew Default = TRUE. Create a new database if the file does not
#' exist, otherwise return fail.
#'
#' @import RSQLite
#'
#' @return A list with 4 elements
#' - success: T/F whether the connection to the database was succesful
#' - statusCode: integer 0-3
#' - msg: The message for each status code
#' @export
#'
dbSetup <- function(
  path,
  schema,
  validateSchema = F,
  showWarning = T,
  createNew = T
) {
  if (!createNew & !file.exists(path)) {
    return(list(
      success = F,
      statusCode = 5,
      msg = sprintf("createNew = FALSE and no database found at %s", path),
      conn = NULL
    ))
  }

  if (!file.exists(path)) {
    # Create a new database
    result <- dbNewFromSchema(path, schema)

    msg <- "A new database was created"
    statusCode <- 0
  } else if (validateSchema) {
    # Check the schema against the provided reference
    result <- dbValidateSchema(path, schema, showWarning)

    if (result$success) {
      statusCode <- 2
      msg <- "Successful connection to existing database; schema validated"
    } else {
      statusCode <- 3
      msg <- result$msg
    }
  } else {
    # Connect without checking the schema
    msg <- "Successful connection to existing database; schema not validated"
    statusCode <- 1
  }

  return(list(
    success = statusCode %in% c(0, 1, 2),
    statusCode = statusCode,
    msg = msg
  ))
}

#' Get a database connection
#'
#' @param dbInfo A path to a database or an existing connection object (DBIConnection or Pool)
#' @param enforceKeyConstraints (Default = TRUE) enforce database key constraints
#'
#' @import RSQLite
#' @importFrom pool localCheckout
#'
#' @return Connection to the database
#' @export
#'
dbGetConn <- function(dbInfo, enforceKeyConstraints = T) {
  # Accept SQLite or Pool
  if (inherits(dbInfo, "DBIConnection")) {
    conn <- dbInfo
    attr(conn, "existing") <- T
  } else if ("Pool" %in% class(dbInfo)) {
    conn <- localCheckout(dbInfo)
    attr(conn, "existing") <- F
  } else if (file.exists(dbInfo)) {
    conn <- dbConnect(SQLite(), dbInfo)
    attr(conn, "existing") <- F
  } else {
    stop("You must provide a path to a database or a connection object")
  }

  if (enforceKeyConstraints) {
    # Make sure that foreign key constraints and cascading are enforced
    q <- dbExecute(conn, "PRAGMA foreign_keys = ON")
  }

  return(conn)
}

#' Finish the current DB operation and disconnect
#'
#' @param conn A database connection
#' @param commit (Default = T) In case the database has an uncommitted transaction
#'
#' @returns Nothing
#' @export
#'
dbFinish <- function(conn, commit = T) {
  if (sqliteIsTransacting(conn) & commit) {
    dbCommit(conn)
  } else if (sqliteIsTransacting(conn)) {
    dbRollback(conn)
  }
  #Pool will auto disconnect with localCheckout, and existing needs to be kept open
  if (!"pool_metadata" %in% names(attributes(conn))) {
    dbDisconnect(conn)
  }
}

#' Create a new SQLite database from a SQL file
#'
#' @param path File path to put the new SQLite Database
#' @param schema Schema to create the database
#' @param data (Default = T) Add data encoded in the file
#'
#' @import RSQLite
#'
#' @returns TRUE if creation was successful, FALSE if file already exists
#' @export
#'
dbNewFromSchema <- function(path, schema, data = T) {
  if (file.exists(path)) {
    return(F)
  }

  statements <- sql_statements(schema)

  # Don't add data
  if (!data) {
    statements <- statements[
      !grepl("^(INSERT|UPDATE|DELETE|REPLACE)", statements)
    ]
  }

  myConn <- dbConnect(SQLite(), path)

  tryCatch(
    {
      q <- sapply(statements, function(sql) {
        q <- dbExecute(myConn, sql)
      })
    },
    error = function(e) {
      dbDisconnect(myConn)
      file.remove(path)
      stop(e)
    }
  )

  dbDisconnect(myConn)

  return(T)
}

#' Check the schema of an existing database againts a reference
#'
#' @param path Path to an existing database
#' @param schema Schema to compare against
#' @param showWarning (Default = TRUE) In case of mismatch, show as warning in console
#'
#' @import RSQLite
#' @importFrom waldo compare
#'
#' @returns List
#' - success: T/F
#' - msg: info
#'
#' @export
dbValidateSchema <- function(path, schema, showWarning = T) {
  if (!file.exists(path)) {
    return(list(success = F, msg = "File does not exist"))
  }

  # Get the schema from the DB at the path
  tryCatch(
    {
      myConn <- dbConnect(SQLite(), path)
      schema1 <- dbGetQuery(
        myConn,
        paste(
          'SELECT sql FROM sqlite_master WHERE type IN ("table", "index")',
          ' AND "sql" NOT NULL AND name != \'sqlite_sequence\''
        )
      ) |>
        paste(collapse = "\n")

      dbDisconnect(myConn)
    },
    error = function(e) {
      return(list(
        success = F,
        msg = "Path does not point to a valid SQLite database"
      ))
    }
  )

  # Get the schema from a blank DB created using the provided schema
  tempDB <- tempfile(fileext = ".db")
  new <- dbNewFromSchema(tempDB, schema)
  myConn <- dbConnect(SQLite(), tempDB)
  schema2 <- dbGetQuery(
    myConn,
    paste(
      'SELECT sql FROM sqlite_master WHERE type IN ("table", "index")',
      ' AND "sql" NOT NULL AND name != \'sqlite_sequence\''
    )
  ) |>
    paste(collapse = "\n")

  dbDisconnect(myConn)

  # Check if schemas match and create diff message if needed
  comparison <- compare(schema1, schema2)
  identical <- length(comparison) == 0

  if (!identical) {
    mismatch <- sprintf(
      "The schemas are not identical\n\n---- DIFFERENCES ----\n\n%s",
      paste(comparison)
    )

    if (showWarning) {
      warning(comparison)
    }
  }

  return(list(
    success = identical,
    msg = ifelse(identical, "Schemas match", mismatch)
  ))
}
