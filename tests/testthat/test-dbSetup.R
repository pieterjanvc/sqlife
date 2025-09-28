test_that("dbSetup", {
  path <- tempfile(fileext = ".db")

  # New DB
  schema <- test_path("testdata", "dummy1.sql")
  result <- dbSetup(path, schema, validateSchema = T)
  expect_equal(result[1:2], list(success = TRUE, statusCode = 0))

  # Existing valid
  result <- dbSetup(path, schema, validateSchema = T)
  expect_equal(result[1:2], list(success = TRUE, statusCode = 2))

  # Existing invalid
  schema <- test_path("testdata", "dummy2.sql")
  expect_warning(result <- dbSetup(path, schema, validateSchema = T))
  expect_equal(result[1:2], list(success = FALSE, statusCode = 3))

  file.remove(path)
})
