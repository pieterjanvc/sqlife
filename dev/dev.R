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

fun1 <- function(x, y) {
  print(missing(x))
  print(missing(y))
}

fun2 <- function(x, y) {
  fun1(x = x, y = y)
}

fun1()
fun2()
