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
