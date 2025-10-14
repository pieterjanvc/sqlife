#' Check if a file is an SQLite database
#'
#' @param path Path to the file
#'
#' @returns TRUE / FALSE
#' @export
#'
dbIsSQLite <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }

  tryCatch(
    {
      conn <- dbConnect(SQLite(), path, synchronous = NULL)
      on.exit(dbDisconnect(conn), add = TRUE)

      # Query sqlite_master, which exists in all valid SQLite DBs
      . <- dbGetQuery(conn, "SELECT name FROM sqlite_master LIMIT 1")

      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}


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
#' @return A list with 3 elements
#' - success: T/F whether the connection to the database was successful
#' - statusCode: Positive integers indicate success, negative indicate failure
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
  if (!file.exists(path)) {
    if (createNew) {
      # Create a new database
      result <- dbNewFromSchema(path, schema)
    }

    return(list(
      success = createNew,
      statusCode = ifelse(createNew, 1, -1),
      msg = ifelse(
        createNew,
        "A new database was created",
        "No database found and createNew = F"
      )
    ))
  }

  if (!dbIsSQLite(path)) {
    return(list(
      success = F,
      statusCode = -2,
      msg = paste(path, "does not point towards a valid SQLite database")
    ))
  }

  # Existing database
  if (validateSchema) {
    # Check the schema against the provided reference
    result <- dbValidateSchema(path, schema, showWarning)

    if (result$success) {
      statusCode <- 2
      msg <- "Successful connection to an existing database; schema validated"
    } else {
      statusCode <- -3
      msg <- "Successful connection to an existing database; schema NOT valid"
    }
  } else {
    # Connect without checking the schema
    msg <- "Successful connection to an existing database; schema not validated"
    statusCode <- 3
  }

  return(list(
    success = statusCode > 0,
    statusCode = statusCode,
    msg = msg
  ))
}

#' Get a database connection
#'
#' @param dbInfo A path to a database or an existing connection object (DBIConnection or Pool)
#' @param enforceKeyConstraints (Default = TRUE) enforce database key constraints
#' @param startTransaction (Default = FALSE) If TRUE, no commit will happen until enforced
#'
#' @import RSQLite
#' @importFrom pool localCheckout
#'
#' @return Connection to the database
#' @export
#'
dbGetConn <- function(dbInfo, enforceKeyConstraints = T, startTransaction = F) {
  # Accept SQLite or Pool
  if (inherits(dbInfo, "DBIConnection")) {
    conn <- dbInfo
    attr(conn, "existing") <- T
  } else if ("Pool" %in% class(dbInfo)) {
    conn <- poolCheckout(dbInfo)
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

  if (startTransaction & !sqliteIsTransacting(conn)) {
    dbBegin(conn)
  }

  return(conn)
}

#' Finish the current DB operation and disconnect
#'
#' @param conn A database connection
#' @param commit (Default = T) In case the database has an uncommitted transaction
#' @param closeExisting (Default = F) Close a previously existing connection.
#' Happens when dbGetConn was invoked with a connection. Otherwise it will auto close.
#' If TRUE and commit = F this will rollback the database if there is an open transaction.
#' @param error (Optional). If set, the database will roll back any transaction
#' and close before throwing an error with the content of this parameter
#'
#' @returns Nothing
#' @export
#'
dbFinish <- function(conn, commit = T, closeExisting = F, error) {
  # Close DB connection (rollback if needed) and throw error
  if (!missing(error)) {
    commit = F
    closeExisting = T
  }

  # Commit or rollback
  transacting <- sqliteIsTransacting(conn)
  changed <- F
  if (transacting & commit) {
    changed <- T
    dbCommit(conn)
    transacting <- F
  } else if (transacting & closeExisting) {
    changed <- F
    dbRollback(conn)
    transacting <- F
  }

  # Close if needed
  closed <- F
  if (closeExisting || !attr(conn, "existing")) {
    closed <- T
    dbDisconnect(conn)
  }

  if (!missing(error)) {
    stop(error)
  }

  return(list(changed = changed, transacting = transacting, closed = closed))
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
