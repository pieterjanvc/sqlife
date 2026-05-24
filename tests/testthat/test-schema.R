test_that("check_names_sql", {
  expect_true(check_names_sql("users"))
  expect_true(check_names_sql("_myTable"))
  expect_true(check_names_sql("my_table_123"))
  expect_false(check_names_sql("my-table"))
  expect_false(check_names_sql("123table"))
  expect_false(check_names_sql(""))
})

test_that("check_names_table", {
  conn <- dbNewFromSchema(schema = test_path("testdata", "dummy1.sql"), memory = ":memory:")$conn
  on.exit(dbFinish(conn, showWarnings = F))

  # Existing tables found
  result <- check_names_table(conn, existing = c("users", "posts"))
  expect_true(result$success)

  # Non-existent table
  result <- check_names_table(conn, existing = "nonexistent")
  expect_false(result$success)
  expect_true(-1 %in% result$statusCode)

  # Valid new name
  result <- check_names_table(conn, new = "new_table")
  expect_true(result$success)

  # Invalid new name
  result <- check_names_table(conn, new = "my-table")
  expect_false(result$success)
  expect_true(-2 %in% result$statusCode)

  # New name already exists
  result <- check_names_table(conn, new = "users")
  expect_false(result$success)
  expect_true(-3 %in% result$statusCode)

  # error = T throws on failure
  expect_error(check_names_table(conn, existing = "nonexistent", error = T))
})

test_that("check_names_attr", {
  conn <- dbNewFromSchema(schema = test_path("testdata", "dummy1.sql"), memory = ":memory:")$conn
  on.exit(dbFinish(conn, showWarnings = F))

  toCheck <- data.frame(
    table = c("users", "users", "users", "login", "comments"),
    attr = c("id", "e-mail", "DOB", "times", "id"),
    new = c(F, F, T, F, T)
  )

  check_names_attr(conn, toCheck, error = F)
  check_names_attr(conn, toCheck |> slice(1, 3, 4), error = F)
})

test_that("schemaInfo keys_only = TRUE returns only PK and FK columns", {
  conn <- dbNewFromSchema(schema = test_path("testdata", "dummy1.sql"), memory = ":memory:")$conn
  on.exit(dbFinish(conn, showWarnings = F))

  result <- schemaInfo(conn, keys_only = TRUE)
  cols <- result$tableInfo

  # Key columns present: id (PK), user_id (FK/PK), post_id (FK), login_time (composite PK)
  expect_true("id"         %in% cols$name)
  expect_true("user_id"    %in% cols$name)
  expect_true("post_id"    %in% cols$name)
  expect_true("login_time" %in% cols$name)

  # Non-key columns absent
  expect_false("username"     %in% cols$name)
  expect_false("email"        %in% cols$name)
  expect_false("title"        %in% cols$name)
  expect_false("content"      %in% cols$name)
  expect_false("comment_text" %in% cols$name)
  expect_false("info"         %in% cols$name)

  # foreignkeyInfo is unaffected by keys_only
  expect_true(nrow(result$foreignkeyInfo) > 0)
})

test_that("schema_rename_table", {
  conn <- dbNewFromSchema(schema = test_path("testdata", "dummy1.sql"), memory = ":memory:")$conn
  on.exit(dbFinish(conn, showWarnings = F))

  schema_rename_table(conn, "users", "members")
  expect_true("members" %in% dbListTables(conn))
  expect_false("users" %in% dbListTables(conn))
})
