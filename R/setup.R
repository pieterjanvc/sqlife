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

  invisible(list(
    success = statusCode > 0,
    statusCode = statusCode,
    msg = msg
  ))
}

#' Get a database connection
#'
#' @param dbInfo A path to a database or an existing connection object
#' (DBIConnection or Pool)
#' @param inherit (Default = TRUE). Inherit (continue with) existing connection
#' if TRUE, create new one if FALSE or if no existing connection
#' @param enforceKeyConstraints (Default = TRUE) enforce database key constraints
#' @param session (Optional) A shiny session in case the connection is returned
#' as part of a reactive object
#'
#' @import RSQLite
#' @importFrom pool poolCheckout
#' @importFrom withr defer
#' @importFrom stats setNames
#'
#' @return Connection to the database
#' @export
#'
dbGetConn <- function(
  dbInfo,
  inherit = T,
  enforceKeyConstraints = T,
  session
) {
  env <- parent.frame()
  parFun <- as.character(sys.call(sys.parent()))[1]
  parentID <- envID(env)

  startTransaction = T
  newConn = !inherit

  # Accept SQLite or Pool
  if (inherits(dbInfo, "DBIConnection") && !newConn) {
    conn <- dbInfo
    check <- attr(conn, "sqlife")$environ[[parentID]]
    attr(conn, "existing") <- ifelse(is.null(check), T, attr(conn, "existing"))
  } else if ("Pool" %in% class(dbInfo)) {
    conn <- poolCheckout(dbInfo)
    attr(conn, "existing") <- F
  } else if (inherits(dbInfo, "DBIConnection") && newConn) {
    if (attr(dbInfo, "memory")) {
      conn <- dbConnect(SQLite(), ":memory:")
      RSQLite::sqliteCopyDatabase(dbInfo, conn)
      attr(conn, "memory") <- T
    } else {
      changed <- dbGetQuery(dbInfo, "SELECT total_changes();")[[1]] != 0

      if (!inherit & changed) {
        warning(
          parFun,
          "is opening a NEW connection from an existing connection ",
          "that is transacting. It cannot write to the database"
        )
      }

      conn <- dbConnect(SQLite(), attr(dbInfo, "dbname"))
      attr(conn, "memory") <- F
    }

    attr(conn, "existing") <- F
    attr(conn, "memory") <- F
  } else if (file.exists(dbInfo)) {
    conn <- dbConnect(SQLite(), dbInfo)
    attr(conn, "existing") <- F
    attr(conn, "memory") <- F
  } else if (is.character(dbInfo) && dbInfo == ":memory:") {
    conn <- dbConnect(SQLite(), dbInfo)
    attr(conn, "existing") <- F
    attr(conn, "memory") <- T
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
      "\n---- DETAILS ----\n",
      "dbConn was called in the ",
      ifelse(parFun == "dbGetConn", "global", parFun),
      " environment which already had a connection object. You can either:\n",
      "- use the exsisting connection directly if not closed\n",
      "- use dbConn with inherit = F to open a new connection and leave the old one untouched",
      "\n-----------------\n"
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

  # This function will run when the environment goes out of scope and will
  #  check if connections were finished properly
  defer(
    {
      envIds <- names(info <- attr(conn, "sqlife")$environ)
      print(envIds)
      idx <- which(parentID == envIds)
      report <- T

      if (idx > 1) {
        info <- attr(conn, "sqlife")$environ[[idx - 1]]
        report <- info$finished
      }

      print(report)

      info <- attr(conn, "sqlife")$environ[[parentID]]
      if (dbIsValid(conn) && !info$finished && info$shiny == 0 & report) {
        if (sqliteIsTransacting(conn)) {
          dbRollback(conn)
        }

        dbDisconnect(conn)
        stop(paste(
          "\n---- DETAILS ----\n",
          info$parFun,
          "environment has an error or is missing dbFinish() before exiting",
          "\n-----------------\n"
        ))
      } else if (info$shiny == 1) {
        session$onSessionEnded(function() {
          if (sqliteIsTransacting(conn)) {
            dbRollback(conn)
            dbDisconnect(conn)
            stop(paste(
              "\n---- DETAILS ----\n",
              info$parFun,
              "Uncommited transactions were found on Shiny session end",
              "\n-----------------\n"
            ))
          }
          message("Closed reactive DB connection")
        })
      }
    },
    envir = env,
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
#' @param new (Default = "commit") Action to perform when the connection was
#' not inherited i.e. new. "commit" will save changes, "revert" will roll back.
#' @param inherit (Default = "continue") Action to perform when the connection
#' was an inherited existing one. Note that the connection will stay open
#' regardless of the option chosen.
#' - continue: keep the transaction open without commit or roll back
#' - commit: commit the transaction
#' - revert: roll back the transaction
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
  new = c("commit", "revert"),
  inherit = c("continue", "commit", "revert"),
  showWarnings = T,
  error
) {
  env = parent.frame()
  parFun = as.character(sys.call(sys.parent()))[1]
  parentID <- envID(env)
  existing <- attributes(conn)$existing
  commit <- ifelse(existing, inherit[1] == "commit", new[1] == "commit")
  continue <- ifelse(existing, inherit[1] == "continue", F)
  closeExisting <- !existing
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
  }

  # Close DB connection (rollback if needed) and throw error
  if (!missing(error)) {
    commit <- F
    continue <- F
    closeExisting <- T
  }

  if (!dbIsValid(conn)) {
    if (!missing(error)) {
      stop(error)
    }

    if (showWarnings) {
      warning("The connection has already been closed or is not valid")
    }
    return(list(changed = F, transacting = F, closed = T))
  }

  # Commit or rollback
  transacting <- sqliteIsTransacting(conn)
  changed <- F
  if (transacting & commit & !continue) {
    # Check if anything changed during the transaction
    changed <- dbGetQuery(conn, "SELECT total_changes();")[[1]] != 0
    dbCommit(conn)
    transacting <- F
  } else if (transacting & closeExisting & !continue) {
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
    stop("\n---- DETAILS ----\n", error, "\n-----------------\n")
  }

  invisible(list(changed = changed, transacting = transacting, closed = closed))
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
    # File connection
    myConn <- dbConnect(SQLite(), path)
    attr(myConn, "memory") <- F
  } else {
    # In memory connection
    myConn <- dbConnect(
      RSQLite::SQLite(),
      ifelse(
        memory == ":memory:",
        memory,
        sprintf("file:%s?mode=memory&cache=shared", memory)
      ),
      uri = memory != ":memory:"
    )

    attr(myConn, "memory") <- T
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

      stop("\n--- SQLite syntax issue ---\n", e)
    }
  )

  if (onDisk & !returnConn) {
    dbDisconnect(myConn)
    myConn <- NULL
  } else {
    # Settings in case the connection is returned
    attr(myConn, "existing") <- F
    attr(myConn, "sqlife") <- new.env()
    attr(myConn, "sqlife")$environ <- attr(myConn, "sqlife")$environ |>
      append(setNames(list(list(finished = F, parFun = parFun)), parentID))
  }

  invisible(list(success = T, conn = myConn))
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
      conn <- dbGetConn(dbInfo, inherit = F)
      schema1 <- dbGetQuery(
        conn,
        paste(
          'SELECT sql FROM sqlite_master WHERE type IN ("table", "index")',
          ' AND "sql" NOT NULL AND name != \'sqlite_sequence\''
        )
      ) |>
        paste(collapse = "\n")

      dbFinish(conn)
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
