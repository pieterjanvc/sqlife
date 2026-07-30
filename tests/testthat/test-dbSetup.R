test_that("dbSetup", {
  path <- tempfile(fileext = ".db")
  on.exit(file.remove(path))
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
  testFun <- function(conn, err, finish = T) {
    if (missing(conn)) {
      conn <- dbGetConn(path)
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
  check <- testFun()
  expect_true("POSIXct" %in% class(check$end))
  expect_equal(check$parFun, "testFun")
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

  # A real error thrown before dbFinish should propagate as-is, not be
  # replaced by the generic "missing dbFinish()" diagnostic (issue #12)
  errFun <- function() {
    conn <- dbGetConn(path)
    stop("some real problem")
    dbFinish(conn)
  }
  caught <- tryCatch(errFun(), error = function(e) e)
  expect_match(conditionMessage(caught), "some real problem", fixed = TRUE)

  # The generic "missing dbFinish()" diagnostic should point back to the
  # dbGetConn() call site (file:line) when source references are available,
  # which testthat provides while running test files (issue #12)
  missingFinishFun <- function() {
    conn <- dbGetConn(path)
    invisible(NULL)
  }
  caught <- tryCatch(missingFinishFun(), error = function(e) e)
  expect_match(
    conditionMessage(caught),
    "dbGetConn\\(\\) was called at test-dbSetup\\.R:[0-9]+"
  )
})
