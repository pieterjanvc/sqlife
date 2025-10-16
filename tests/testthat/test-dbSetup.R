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

  file.remove(path)

  # Check the in-memory database
  result <- dbNewFromSchema(schema = schema, memory = "test")
  expect_equal(length(tbl(result$conn, "users") |> pull(username)), 3)
  expect_equal(dbValidateSchema(result$conn, schema = schema)$success, T)
  dbDisconnect(result$conn)
})
