test_that("dbSetup", {
  path <- tempfile(fileext = ".db")

  # New DB
  schema <- test_path("testdata", "dummy1.sql")
  result <- dbSetup(path, schema, validateSchema = T)
  expect_equal(result[1:2], list(success = TRUE, statusCode = 1))

  # New but with createNew = F
  result <- dbSetup(test_path("testdata", "dummy.sql"), createNew = F)
  expect_equal(result[1:2], list(success = FALSE, statusCode = -1))

  # Existing invalid DB
  result <- dbSetup(schema)
  expect_equal(result[1:2], list(success = FALSE, statusCode = -2))

  # Existing valid DB and schema
  result <- dbSetup(path, schema, validateSchema = T)
  expect_equal(result[1:2], list(success = TRUE, statusCode = 2))

  # Existing valid DB but schema not checked
  result <- dbSetup(path, schema, validateSchema = F)
  expect_equal(result[1:2], list(success = TRUE, statusCode = 3))

  # Existing valid DB invalid schema
  schema <- test_path("testdata", "dummy2.sql")
  expect_warning(result <- dbSetup(path, schema, validateSchema = T))
  expect_equal(result[1:2], list(success = FALSE, statusCode = -3))

  # Check the in-memory database
  result <- dbNewFromSchema(schema = schema, memory = "test")
  expect_equal(length(tbl(result$conn, "users") |> pull(username)), 3)
  expect_equal(dbValidateSchema(result$conn, schema = schema)$success, T)
  dbDisconnect(result$conn)

  # Correct useage of dbGetConn and dbFininsh
  testFun <- function(conn, err, addNew = F, finish = T) {
    if (missing(conn)) {
      conn <- dbGetConn(path)
    }

    # Opening connection again
    if (addNew) {
      conn <- dbGetConn(conn)
    }

    . <- tbl(conn, "users") |> collect()

    # Exiting the function (error or early return) before dbFinish
    if (!missing(err)) {
      return(err)
    }

    if (finish) {
      dbFinish(conn)
    }
  }

  # Start new connection and close it before environment ends
  expect_equal(testFun(), list(changed = F, transacting = F, closed = T))
  # Run dbGetConn more than once in the same environment
  expect_warning(testFun(addNew = T))
  # Forget to finish a connection before the environment ends
  expect_error(testFun(finish = F))
  # Error or return without dbFinish will close the connection and throw error
  expect_error(testFun(err = T))
  # Test with existing connection being passed
  conn <- dbGetConn(path)
  #Passing existing connection
  expect_invisible(testFun(conn, finish = F))
  expect_identical(dbIsValid(conn), T)
  #Trying to run dbFinish on existing connection inside of child environment
  expect_error(testFun(conn))
  expect_identical(dbIsValid(conn), F)

  file.remove(path)
})
