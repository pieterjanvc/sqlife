test_that("schema_dbml generates correct DBML output", {
  conn <- dbNewFromSchema(schema = test_path("testdata", "dummy1.sql"), memory = ":memory:")$conn
  on.exit(dbFinish(conn, showWarnings = F))

  result <- schema_dbml(conn)

  expect_type(result, "character")
  expect_true(nchar(result) > 0)

  # All four table blocks are present (simple names are unquoted)
  expect_true(grepl('Table users',    result, fixed = TRUE))
  expect_true(grepl('Table posts',    result, fixed = TRUE))
  expect_true(grepl('Table comments', result, fixed = TRUE))
  expect_true(grepl('Table login',    result, fixed = TRUE))

  # INTEGER PRIMARY KEY AUTOINCREMENT renders as [pk, increment]
  expect_true(grepl('id INTEGER [pk, increment]', result, fixed = TRUE))

  # Explicit NOT NULL column
  expect_true(grepl('username TEXT [not null]', result, fixed = TRUE))

  # Nullable column with no settings
  expect_true(grepl('email TEXT\n', result, fixed = TRUE))

  # Typeless column (login.user_id has no type in DDL) falls back to varchar;
  # composite PK suppresses the [pk] setting on the column itself
  expect_true(grepl('user_id varchar\n', result, fixed = TRUE))

  # Composite PK goes in the indexes block, not on individual columns
  expect_true(grepl('(user_id, login_time) [pk]', result, fixed = TRUE))

  # Foreign key Ref statements (many-to-one direction)
  expect_true(grepl('Ref: posts.user_id > users.id',       result, fixed = TRUE))
  expect_true(grepl('Ref: comments.post_id > posts.id',    result, fixed = TRUE))
  expect_true(grepl('Ref: login.user_id > users.id',       result, fixed = TRUE))

  # Project block with database_type (single-quoted string) and note
  result_proj <- schema_dbml(conn, project_name = "mydb", note = "test note")
  expect_true(grepl('Project mydb',            result_proj, fixed = TRUE))
  expect_true(grepl("database_type: 'SQLite'", result_proj, fixed = TRUE))
  expect_true(grepl("note: 'test note'",       result_proj, fixed = TRUE))

  # schema_dbml_embed returns a dbdiagram.io URL containing the base64-encoded DBML
  url <- schema_dbml_embed(result)
  expect_type(url, "character")
  expect_true(startsWith(url, "https://dbdiagram.io/embed?c="))
  decoded <- rawToChar(base64enc::base64decode(URLdecode(sub("^.*\\?c=", "", url))))
  expect_equal(decoded, result)
})

test_that("schema_dbml_embed encodes DBML as a valid dbdiagram.io URL", {
  conn <- dbNewFromSchema(schema = test_path("testdata", "dummy1.sql"), memory = ":memory:")$conn
  on.exit(dbFinish(conn, showWarnings = F))

  dbml <- schema_dbml(conn)
  url  <- schema_dbml_embed(dbml)

  # Returns a character string starting with the expected base URL
  expect_type(url, "character")
  expect_true(startsWith(url, "https://dbdiagram.io/embed?c="))

  # Decoding the query parameter must recover the original DBML exactly
  encoded <- sub("^https://dbdiagram\\.io/embed\\?c=", "", url)
  decoded  <- rawToChar(base64enc::base64decode(URLdecode(encoded)))
  expect_equal(decoded, dbml)

  # URL is within the 8000-character limit for the test database
  expect_lt(nchar(url), 8000)
})

test_that("schema_dbml_iframe produces correct HTML with default parameters", {
  conn <- dbNewFromSchema(schema = test_path("testdata", "dummy1.sql"), memory = ":memory:")$conn
  on.exit(dbFinish(conn, showWarnings = F))

  url    <- schema_dbml_embed(schema_dbml(conn))
  iframe <- schema_dbml_iframe(url)

  expect_type(iframe, "character")
  expect_true(startsWith(iframe, "<iframe\n"))
  expect_true(endsWith(iframe,   "\n></iframe>"))
  expect_true(grepl(paste0('src="', url, '"'),  iframe, fixed = TRUE))
  expect_true(grepl('width="100%"',             iframe, fixed = TRUE))
  expect_true(grepl('height="600"',             iframe, fixed = TRUE))
  expect_true(grepl('style="border: 0"',        iframe, fixed = TRUE))
  expect_true(grepl('loading="lazy"',           iframe, fixed = TRUE))
  expect_true(grepl("allowfullscreen",           iframe, fixed = TRUE))
})

test_that("schema_dbml keys_only = FALSE includes all columns in DBML output", {
  conn <- dbNewFromSchema(schema = test_path("testdata", "dummy1.sql"), memory = ":memory:")$conn
  on.exit(dbFinish(conn, showWarnings = F))

  result <- schema_dbml(conn, keys_only = FALSE)

  # Non-key columns must be present
  expect_true(grepl('username',     result, fixed = TRUE))
  expect_true(grepl('email',        result, fixed = TRUE))
  expect_true(grepl('title',        result, fixed = TRUE))
  expect_true(grepl('content',      result, fixed = TRUE))
  expect_true(grepl('comment_text', result, fixed = TRUE))
  expect_true(grepl('info',         result, fixed = TRUE))
})

test_that("schema_dbml keys_only = TRUE omits non-key columns from DBML output", {
  conn <- dbNewFromSchema(schema = test_path("testdata", "dummy1.sql"), memory = ":memory:")$conn
  on.exit(dbFinish(conn, showWarnings = F))

  result <- schema_dbml(conn, keys_only = TRUE)

  # Key columns (PK / FK) must still appear
  expect_true(grepl('id INTEGER [pk, increment]', result, fixed = TRUE))
  expect_true(grepl('user_id',    result, fixed = TRUE))
  expect_true(grepl('post_id',    result, fixed = TRUE))
  expect_true(grepl('login_time', result, fixed = TRUE))

  # Non-key columns must be absent
  expect_false(grepl('username',     result, fixed = TRUE))
  expect_false(grepl('email',        result, fixed = TRUE))
  expect_false(grepl('title',        result, fixed = TRUE))
  expect_false(grepl('content',      result, fixed = TRUE))
  expect_false(grepl('comment_text', result, fixed = TRUE))
  expect_false(grepl('info',         result, fixed = TRUE))

  # Ref lines are still generated
  expect_true(grepl('Ref: posts.user_id > users.id',    result, fixed = TRUE))
  expect_true(grepl('Ref: comments.post_id > posts.id', result, fixed = TRUE))
  expect_true(grepl('Ref: login.user_id > users.id',    result, fixed = TRUE))
})

