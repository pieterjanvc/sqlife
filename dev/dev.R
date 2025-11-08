dbInfo <- "local/test.db"
schema <- "tests/testthat/testdata/dummy1.sql"
dbSetup(dbInfo, schema, validateSchema = T)

dbNewFromSchema("C:/Users/pj/Desktop/testtest.db", schema = schema)


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

fun3 <- function(env) {
  print("start3")
  withr::defer(
    expr = {
      print("Clean up")
    },
    envir = env,
    priority = "last"
  )
}

fun2 <- function(dbInfo) {
  conn2 <- dbConn_inherit(dbInfo)
  dbFinish(conn2)
}

fun1 <- function() {
  print("start1")
  conn <- dbConn_new(dbInfo)
  fun2(dbInfo)
  dbIsValid(conn) |> print()
  dbFinish(conn) |> print()
  print("end1")
}

fun1()
