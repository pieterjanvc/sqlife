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
# - commit / rollback / new transaction / keep open / close
