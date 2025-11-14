#' Get a connection from a dbInfo object
#'
#' @param dbInfo dbInfo object
#' @param startTransaction If TRUE, a new transaction is started if the existing
#' connection is not transacting yet.
#' @param constraints Ignored when connection is passed.Enforce foreign key
#'constraints
#' @param busyTimeoutIgnored when connection is passed. Set the timeout in
#' milliseconds when a new connection is created from a path. This
#' is only needed if concurrency is anticipated
#' @param silentErr (Default = FALSE). If set to TRUE, no error will be
#' returned if the environment exits before dbFinish has been run.
#'
#' @returns An SQLite connection
#' @export
#'
dbGetConnFromInfo <- function(
  dbInfo,
  startTransaction,
  constraints,
  busyTimeout,
  silentErr = F
) {
  if ("SQLiteConnection" %in% class(dbInfo)) {
    # Pass on existing connection
    conn <- dbInfo
    attr(conn, "sqlife")$info$dbInfo <- "connection"

    if (startTransaction & !sqliteIsTransacting(conn)) {
      dbBegin(conn)
    }
  } else {
    # # Get new connection
    # if (!missing(startTransaction) && startTransaction) {
    #   stop(
    #     "When providing a path to a database commit must be TRUE\n",
    #     "Pass a connection if you want to start a transaction that does",
    #     "not commit"
    #   )
    # }

    conn <- dbGetConn(
      dbInfo,
      enforceKeyConstraints = constraints,
      busyTimeout = busyTimeout,
      env = parent.frame(),
      parFun = as.character(sys.call(sys.parent()))[1],
      silentErr = silentErr
    )
    attr(conn, "sqlife")$info$dbInfo <- "path"

    if (startTransaction) {
      dbBegin(conn)
    }
  }

  attr(conn, "sqlife")$info$nested <- attr(conn, "sqlife")$info$nested + 1

  return(conn)
}

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
    conn <- dbGetConn(path)
    result <- dbValidateSchema(conn, schema, showWarning)
    dbFinish(conn)

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
#' @param busyTimeout (Default = 0) Time in milliseconds to wait before returning
#' an error if the database is locked (Useful in case of anticipated concurrency)
#' @param session (Optional) A shiny session in case the connection is returned
#' as part of a reactive object
#'
#' @import RSQLite
#' @importFrom withr defer
#'
#' @return Connection to the SQLite database
#' @export
#'
dbGetConn <- function(
  path,
  enforceKeyConstraints = T,
  busyTimeout = 0,
  session,
  ...
) {
  extra <- list(...)

  if (is.null(extra$env)) {
    env <- parent.frame()
  } else {
    env <- extra$env
  }

  if (is.null(extra$parFun)) {
    parFun <- as.character(sys.call(sys.parent()))[1]
  } else {
    parFun <- extra$parFun
  }

  parentID <- envID(env)
  reactive <- !missing(session)

  # Get connection
  if (file.exists(path)) {
    conn <- dbConnect(SQLite(), path)
    attr(conn, "memory") <- F
  } else if (is.character(path) && path == ":memory:") {
    conn <- dbConnect(SQLite(), ":memory:")
    attr(conn, "memory") <- T
  } else {
    stop("You must provide a path to a database or ':memory:'")
  }

  # Create environment to track stats
  attr(conn, "sqlife") <- new.env()
  attr(conn, "sqlife")$info <- list(
    start = Sys.time(),
    parentID = parentID,
    parFun = parFun,
    nested = 0,
    silentErr = ifelse(is.null(extra$silentErr), F, extra$silentErr),
    shiny = ifelse(reactive, T, F),
    transacting = F,
    timeInTransaction = 0,
    end = NULL
  )

  # This function will run when the environment goes out of scope and will
  #  check if connection was finished properly
  defer(
    {
      info <- attr(conn, "sqlife")$info

      if (dbIsValid(conn) && is.null(info$end) && !info$shiny) {
        if (sqliteIsTransacting(conn)) {
          dbRollback(conn)
        }

        dbDisconnect(conn)

        if (!info$silentErr) {
          stop(paste(
            "\n---- DETAILS ----\n",
            info$parFun,
            "environment has an error or is missing dbFinish() before exiting",
            "\n-----------------\n"
          ))
        }
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
          } else {
            dbDisconnect(conn)
          }
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

  if (busyTimeout > 0) {
    q <- dbExecute(
      conn,
      sprintf("PRAGMA busy_timeout = %i", as.integer(busyTimeout))
    )
  }

  return(conn)
}

#' Finish the current DB operation and disconnect
#'
#' dbFinish() must be called in the same environment as dbGetConn() to
#' make sure errors are handled correctly
#'
#' @param conn A database connection
#' @param commit (Default = "TRUE") Commit any open transaction
#'
#' @import RSQLite
#'
#' @returns Sqlife connection info list
#' @export
#'
dbFinish <- function(
  conn,
  commit = T,
  ...
) {
  if (is.null(attr(conn, "sqlife"))) {
    stop(
      "\n---- DETAILS ----\nconnection was not opened with dbGetConn()",
      "\n-----------------\n"
    )
  }

  # if (!is.null(attr(conn, "sqlife")$info$dbInfo)) {
  #   stop(
  #     "dbFinishFromInfo must be called for a connection opened with dbGetConnFromInfo"
  #   )
  # }

  if (attr(conn, "sqlife")$info$nested > 0) {
    stop(
      "dbFinishFromInfo must be called for a connection opened with dbGetConnFromInfo"
    )
  }

  extra <- list(...)

  if (is.null(extra$env)) {
    env <- parent.frame()
  } else {
    env <- extra$env
  }

  if (is.null(extra$parFun)) {
    parFun <- as.character(sys.call(sys.parent()))[1]
  } else {
    parFun <- extra$parFun
  }

  parentID <- envID(env)
  info <- attr(conn, "sqlife")$info

  # Must be an open connection
  if (!dbIsValid(conn)) {
    warning(
      "The connection in ",
      parFun,
      "was already closed by the time dbFinish was called"
    )
    return(invisible(info))
  }

  # Finish must be called in same environment where connection was opened
  if (info$parentID != parentID) {
    if (sqliteIsTransacting(conn)) {
      dbRollback(conn)
    }

    dbDisconnect(conn)
    # Mark the environment as finished
    attr(conn, "sqlife")$info[["end"]] <- Sys.time()

    stop(
      "\n---- DETAILS ----\ndbFinish is called in the ",
      parFun,
      " environment on a connection opened in another\n-----------------\n"
    )
  }

  # Commit or roll back
  if (sqliteIsTransacting(conn) && commit) {
    dbCommit(conn)
  }

  if (sqliteIsTransacting(conn) && !commit) {
    dbRollback(conn)
  }

  dbDisconnect(conn)
  # Mark the environment as finished
  attr(conn, "sqlife")$info[["end"]] <- Sys.time()

  invisible(attr(conn, "sqlife")$info)
}


#' Finish a DB interaction started with dbGetConnFromInfo
#'
#' @param conn Connection object
#' @param commit Commit changes
#' @param showWarning (Default = TRUE) Show warnings
#'
#' @returns Database connection sqlife info
#' @export
#'
dbFinishFromInfo <- function(conn, commit, showWarning = T) {
  type <- attr(conn, "sqlife")$info$dbInfo
  nested <- attr(conn, "sqlife")$info$nested

  if (nested == 0 & is.null(type)) {
    stop(
      "dbFinish must be called for a connection opened with dbGetConn"
    )
  }

  #Reset the dbInfo again
  if (nested == 1) {
    attr(conn, "sqlife")$info$dbInfo <- NULL
  }

  attr(conn, "sqlife")$info$nested <- nested - 1

  if (nested == 1 && type == "path") {
    # New connections from path must commit or roll back and close
    if (showWarning && !commit) {
      warning(
        "Providing a path to tbl_insert with commit = F will not insert",
        "any new data just check if it's possible"
      )
    }

    dbFinish(
      conn,
      commit,
      env = parent.frame(),
      parFun = as.character(sys.call(sys.parent()))[1]
    )
  } else if (commit) {
    # Existing connections only commit if set to do
    if (sqliteIsTransacting(conn)) {
      dbCommit(conn)
    }
  }

  return(invisible(attr(conn, "sqlife")$info))
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
    attr(myConn, "sqlife")$info <- list(
      start = Sys.time(),
      parentID = parentID,
      parFun = parFun,
      nested = 0,
      silentErr = F,
      shiny = F,
      transacting = F,
      timeInTransaction = 0,
      end = NULL
    )
  }

  invisible(list(success = T, conn = myConn))
}

#' Check the schema of an existing database againts a reference
#'
#' @param conn connection object
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
dbValidateSchema <- function(conn, schema, showWarning = T) {
  # Get the schema from the DB
  tryCatch(
    {
      schema1 <- dbGetQuery(
        conn,
        paste(
          'SELECT sql FROM sqlite_master WHERE type IN ("table", "index")',
          ' AND "sql" NOT NULL AND name != \'sqlite_sequence\''
        )
      ) |>
        paste(collapse = "\n")
    },
    error = function(e) {
      return(list(
        success = F,
        msg = "The connection does not point to a valid SQLite database"
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
