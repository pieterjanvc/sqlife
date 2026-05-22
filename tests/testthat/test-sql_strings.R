test_that("sql_statement_schema", {
  conn <- dbNewFromSchema(schema = test_path("testdata", "dummy1.sql"), memory = ":memory:")$conn
  on.exit(dbFinish(conn, showWarnings = F))

  # Returns named vector, one entry per table
  result <- sql_statement_schema(conn)
  expect_type(result, "character")
  expect_true("users" %in% names(result))
  expect_match(result[["users"]], "CREATE TABLE")

  # include filters to specific tables
  result <- sql_statement_schema(conn, include = c("users", "posts"))
  expect_equal(length(result), 2)

  # collapse returns a single string
  result <- sql_statement_schema(conn, collapse = T)
  expect_length(result, 1)

  # error on unknown table
  expect_error(sql_statement_schema(conn, include = "nonexistent"))
})

test_that("sql_statement_data", {
  conn <- dbNewFromSchema(schema = test_path("testdata", "dummy1.sql"), memory = ":memory:")$conn
  on.exit(dbFinish(conn, showWarnings = F))

  # Returns named result with INSERT statements (list when some tables are empty)
  result <- sql_statement_data(conn)
  expect_true("users" %in% names(result))
  expect_match(result[["users"]], "INSERT INTO")

  # include filters to specific tables
  result <- sql_statement_data(conn, include = "users")
  expect_equal(length(result), 1)

  # collapse returns a single string
  result <- sql_statement_data(conn, collapse = T)
  expect_length(result, 1)

  # error on unknown table
  expect_error(sql_statement_data(conn, include = "nonexistent"))
})
