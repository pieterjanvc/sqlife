conn <- dbGetConn("../CFME/local/cfme.db")

dbInfo <- "local/test.db"
schema <- "tests/testthat/testdata/dummy1.sql"
dbSetup(dbInfo, schema, validateSchema = T)

dbNewFromSchema("C:/Users/pj/Desktop/testtest.db", schema = schema)
# devtools::install_github("pieterjanvc/sqlife", ref = "expandConnections")

test <- function(memory) {
  onDisk <- missing(memory)
  tryCatch(
    {
      stop("Issue")
    },
    error = function(e) {
      if (onDisk) {
        print("del")
      }

      stop(e)
    }
  )
}

test(memory = "OK")


x <- dbConnect(SQLite(), dbInfo)
attr(x, "dbname")
dbDisconnect(x)

dbInfo <- "D:/Desktop/test1.db"

fun1 <- function(dbInfo) {
  conn <- dbGetConn(dbInfo)
  tryCatch(
    {
      tbl_insert(data.frame(usernam = "test"), conn, "users", returnData = F)
    },
    error = function(e) {
      dbFinish(conn, error = e)
    }
  )

  fun2(dbInfo)
  dbFinish(conn, new = "revert")
}

fun2 <- function(dbInfo) {
  conn <- dbGetConn(dbInfo)
  tbl(conn, "users") |> collect() |> print()
  dbFinish(conn)
}


conn <- dbGetConn(dbInfo)
fun1(conn)
dbFinish(conn, new = "revert")


dbInfo <- "local/test.db"
schema <- "tests/testthat/testdata/dummy1.sql"
dbSetup(dbInfo, schema, validateSchema = T)

conn <- dbConnect(SQLite(), dbInfo)
dbBegin(conn)

fun1 <- function(conn) {
  dbGetQuery(conn, "INSERT INTO users(username) VALUES('test') RETURN *")
}

fun1(conn)

sqliteIsTransacting(conn)
dbCommit(conn)

# Maybe dbStatus()

fun1 <- function(conn) {
  tbl_insert(data.frame(username = "test5"), conn, "users")
  dbFinish(conn)
}

fun2 <- function(...) {
  conn <- dbGetConn(dbInfo)
  fun1(conn)
  print(dbFinish(conn))
}
fun2(tes = 5)


dbFinishFromInfo <- function(conn, commit, showWarning = T) {
  type <- attr(conn, "sqlife")$info$dbInfo

  if (is.null(type)) {
    stop(
      "dbFinishFromInfo must be called for a connection opened with dbConnFromInfo"
    )
  }

  #Reset the dbInfo again
  attr(conn, "sqlife")$info$dbInfo <- NULL

  if (type == "path") {
    # New connections from path must commit or roll back and close
    if (showWarning && !commit) {
      warning(
        "Providing a path to tbl_insert with commit = F will not insert",
        "any new data just check if it's possible"
      )
    }
    dbFinish(conn, commit)
  } else if (commit) {
    # Existing connections only commit if set to do
    if (sqliteIsTransacting(conn)) {
      dbCommit(conn)
    }
  }
}

# check <- keyCheck(schemainfo)
# if (check$statusCode < 0) {
#   stop(
#     "The schema has the following issues\nPrimary Keys\n",
#     dfAsText(check$PKcheck |> filter(!hasPK)),
#     "\n\nForeign Keys\n",
#     dfAsText(check$FKcheck |> filter(issue))
#   )
# }

# schemainfo <- schemaInfo(conn)
