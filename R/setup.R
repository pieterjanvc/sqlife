#' Check if a file / connection is an SQLite database
#'
#' @param dbInfo Path to the file
#'
#' @returns TRUE / FALSE
#' @export
#'
dbIsSQLite <- function(dbInfo) {
  if ("SQLiteConnection" %in% class(dbInfo)) {
    return(TRUE)
  }

  if (!file.exists(dbInfo)) {
    return(FALSE)
  }

  tryCatch(
    {
      conn <- dbConnect(SQLite(), dbInfo, synchronous = NULL)
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
    normalizePath(dirname(path), mustWork = T)

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
#' @param dbInfo A path to a database or an existing connection object
#' (DBIConnection or Pool)
#' @param enforceKeyConstraints (Default = TRUE) enforce database key constraints
#' @param startTransaction (Default = FALSE) If TRUE, no commit will happen
#' until enforced
#' @param newConn (Default = F). If true, a new connection will be created if
#' there is an existing connection passed. This can handy for side transactions
#' if the original one is transacting and should not be disturbed
#' @param session (Optional) A shiny session in case the connection is returned
#' as part of a reactive object
#'
#' @import RSQLite
#' @importFrom pool poolCheckout
#' @importFrom withr defer_parent
#' @importFrom stats setNames
#'
#' @return Connection to the database
#' @export
#'
dbGetConn <- function(
  dbInfo,
  enforceKeyConstraints = T,
  startTransaction = F,
  # readOnly = F,
  newConn = F,
  session
) {
  env = parent.frame()
  parFun = as.character(sys.call(sys.parent()))[1]
  parentID <- envID(env)
  # access <- ifelse(readOnly, SQLITE_RO,SQLITE_RW)

  # Accept SQLite or Pool
  if (inherits(dbInfo, "DBIConnection") && !newConn) {
    conn <- dbInfo
    check <- attr(conn, "sqlife")$environ[[parentID]]
    attr(conn, "existing") <- ifelse(is.null(check), T, attr(conn, "existing"))
  } else if ("Pool" %in% class(dbInfo)) {
    conn <- poolCheckout(dbInfo)
    attr(conn, "existing") <- F
  } else if (inherits(dbInfo, "DBIConnection") && newConn) {
    conn <- dbConnect(SQLite(), attr(dbInfo, "dbname"))
    attr(conn, "existing") <- F
  } else if (file.exists(dbInfo)) {
    conn <- dbConnect(SQLite(), dbInfo)
    attr(conn, "existing") <- F
  } else if (is.character(dbInfo) && dbInfo == ":memory:") {
    conn <- dbConnect(SQLite(), dbInfo)
    attr(conn, "existing") <- F
  } else {
    stop("You must provide a path to a database or a connection object")
  }

  # Make sure that when the connection goes out of score with no dbFinish
  # an error is raised and the connection is closed
  parentID <- envID(env)
  reactive <- !missing(session)

  if (!is.null(attr(conn, "sqlife")$environ[[parentID]])) {
    if (attr(conn, "sqlife")$environ[[parentID]]$shiny > 0) {
      attr(conn, "sqlife")$environ[[parentID]]$shiny <- 2
    }

    warning(
      "dbGetConn is called multiple times in the ",
      ifelse(parFun == "dbGetConn", "global", parFun),
      " environment"
    )
  } else {
    if (is.null(attr(conn, "sqlife")$environ)) {
      attr(conn, "sqlife") <- new.env()
    }
    attr(conn, "sqlife")$environ <- attr(conn, "sqlife")$environ |>
      append(setNames(
        list(list(
          finished = F,
          parFun = parFun,
          shiny = ifelse(reactive, 1, 0)
        )),
        parentID
      ))
  }

  defer_parent(
    {
      info <- attr(conn, "sqlife")$environ[[parentID]]
      if (dbIsValid(conn) && !info$finished && info$shiny == 0) {
        if (sqliteIsTransacting(conn)) {
          dbRollback(conn)
        }

        dbDisconnect(conn)
        stop(paste(
          info$parFun,
          "environment is missing dbFinish() before exiting the environment"
        ))
      } else if (info$shiny == 1) {
        session$onSessionEnded(function() {
          if (sqliteIsTransacting(conn)) {
            dbRollback(conn)
            dbDisconnect(conn)
            stop(paste(
              info$parFun,
              "Uncommited transactions were found on Shiny session end"
            ))
          }
          message("Closed reactive DB connection")
        })
      }
    },
    priority = "last"
  )

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
#' @param commit (Default = T) In case the database has an uncommitted transaction.
#' FALSE will roll back
#' @param closeExisting (Default = F) Close a previously existing connection.
#' Happens when dbGetConn was invoked with a connection. Otherwise it will auto close.
#' If TRUE and commit = F this will rollback the database if there is an open transaction.
#' @param showWarnings (Default = T) Show warning messages
#' @param error (Optional). If set, the database will roll back any transaction
#' and close before throwing an error with the content of this parameter
#'
#' @import RSQLite
#'
#' @returns Nothing
#' @export
#'
dbFinish <- function(
  conn,
  commit = T,
  closeExisting = F,
  showWarnings = T,
  error
) {
  env = parent.frame()
  parFun = as.character(sys.call(sys.parent()))[1]
  parentID <- envID(env)
  closed <- F

  if (missing(error) && is.null(attr(conn, "sqlife")$environ[[parentID]])) {
    # Check that we're finishing in the same environment as started
    check <- parentID != names(attr(conn, "sqlife")$environ)[[1]]

    if (check) {
      orgEnv <- attr(conn, "sqlife")$environ[[1]]$parFun
      error <- paste(
        "dbFinish cannot be called inside",
        parFun,
        "as the connection was opened in",
        ifelse(orgEnv == "dbGetConn", "the global environment", orgEnv),
        "and should be closed there"
      )
    } else {
      error <- paste(
        "dbFinish was called inside",
        parFun,
        "without dbGetConn in the same environment"
      )
    }
  }

  if (is.null(attributes(conn)$existing)) {
    error <- paste(
      "Database connection was not opened with dbGetConn and cannot",
      "be handled properly by dbFinish. Rollback and close with error."
    )
  } else if (!attributes(conn)$existing & !commit) {
    error <- "Only existing connections can have commit = F"
  }

  # Close DB connection (rollback if needed) and throw error
  if (!missing(error)) {
    commit <- F
    closeExisting <- T
  }

  if (!dbIsValid(conn)) {
    if (showWarnings) {
      warning("The connection has already been closed or is not valid")
    }

    return(list(changed = F, transacting = F, closed = T))
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

  if (closeExisting || !attr(conn, "existing")) {
    closed <- T
    dbDisconnect(conn)
  }

  # Mark the environment as finished
  attr(conn, "sqlife")$environ[[parentID]][["finished"]] <- T

  if (!missing(error)) {
    stop("\n---- DETAILS ----\n", error, "\n-----------------\n\n")
  }

  return(list(changed = changed, transacting = transacting, closed = closed))
}

#' Create a new SQLite database from a SQL file
#'
#' @param path File path to put the new SQLite Database
#' @param schema Schema to create the database
#' @param data (Default = T) Add data encoded in the file
#' @param returnConn (Default = F) Return an open connection
#' @param memory (optional) If set, path is ignored and an in-memory database
#' with the name provided in memory argument is created and a connection returned.
#' IN case of :memory: a simple memory database is created, otherwise a shared
#' memory SQLite database with the provided name is created
#'
#'
#' @import RSQLite
#' @importFrom stats setNames
#'
#' @returns list(success, conn)
#' @export
#'
dbNewFromSchema <- function(
  path,
  schema,
  data = T,
  returnConn = F,
  memory
) {
  onDisk <- missing(memory)
  env = parent.frame()
  parFun = as.character(sys.call(sys.parent()))[1]
  parentID <- envID(env)

  if (onDisk && file.exists(path)) {
    return(list(success = F, conn = NULL))
  }

  statements <- sql_statements(schema)

  # Don't add data
  if (!data) {
    statements <- statements[
      !grepl("^(INSERT|UPDATE|DELETE|REPLACE)", statements)
    ]
  }

  if (onDisk) {
    myConn <- dbConnect(SQLite(), path)
  } else {
    myConn <- dbConnect(
      RSQLite::SQLite(),
      ifelse(
        memory == ":memory:",
        memory,
        sprintf("file:%s?mode=memory&cache=shared", memory)
      ),
      uri = memory != ":memory:"
    )
  }

  tryCatch(
    {
      q <- sapply(statements, function(sql) {
        q <- dbExecute(myConn, sql)
      })
    },
    error = function(e) {
      dbDisconnect(myConn)
      if (onDisk) {
        file.remove(path)
      }

      stop("\n--- SQLite syntax issue ---\n\n", e)
    }
  )

  if (onDisk & !returnConn) {
    dbDisconnect(myConn)
    myConn <- NULL
  } else {
    attr(myConn, "existing") <- F
    attr(myConn, "sqlife") <- new.env()
    attr(myConn, "sqlife")$environ <- attr(myConn, "sqlife")$environ |>
      append(setNames(list(list(finished = F, parFun = parFun)), parentID))
  }

  return(return(list(success = T, conn = myConn)))
}

#' Check the schema of an existing database againts a reference
#'
#' @param dbInfo dbInfo object
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
dbValidateSchema <- function(dbInfo, schema, showWarning = T) {
  # Get the schema from the DB
  tryCatch(
    {
      conn <- dbGetConn(dbInfo)
      schema1 <- dbGetQuery(
        conn,
        paste(
          'SELECT sql FROM sqlite_master WHERE type IN ("table", "index")',
          ' AND "sql" NOT NULL AND name != \'sqlite_sequence\''
        )
      ) |>
        paste(collapse = "\n")

      dbFinish(conn, commit = F)
    },
    error = function(e) {
      return(list(
        success = F,
        msg = "dbInfo does not point to a valid SQLite database"
      ))
    }
  )

  # Get the schema from a blank DB created using the provided schema
  tempConn <- dbNewFromSchema(schema = schema, memory = ":memory:")$conn
  schema2 <- dbGetQuery(
    tempConn,
    paste(
      'SELECT sql FROM sqlite_master WHERE type IN ("table", "index")',
      ' AND "sql" NOT NULL AND name != \'sqlite_sequence\''
    )
  ) |>
    paste(collapse = "\n")

  dbDisconnect(tempConn)

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
