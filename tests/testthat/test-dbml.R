test_that("schema_dbml generates correct DBML output", {
  conn <- dbNewFromSchema(schema = test_path("testdata", "dummy1.sql"), memory = ":memory:")$conn
  on.exit(dbFinish(conn, showWarnings = F))

  result <- schema_dbml(conn)

  expect_type(result, "character")
  expect_true(nchar(result) > 0)

  # All four table blocks are present
  expect_true(grepl('Table "users"',    result, fixed = TRUE))
  expect_true(grepl('Table "posts"',    result, fixed = TRUE))
  expect_true(grepl('Table "comments"', result, fixed = TRUE))
  expect_true(grepl('Table "login"',    result, fixed = TRUE))

  # INTEGER PRIMARY KEY AUTOINCREMENT renders as [pk, increment]
  expect_true(grepl('"id" INTEGER [pk, increment]', result, fixed = TRUE))

  # Explicit NOT NULL column
  expect_true(grepl('"username" TEXT [not null]', result, fixed = TRUE))

  # Nullable column with no settings
  expect_true(grepl('"email" TEXT\n', result, fixed = TRUE))

  # Typeless column (login.user_id has no type in DDL) falls back to varchar;
  # composite PK suppresses the [pk] setting on the column itself
  expect_true(grepl('"user_id" varchar\n', result, fixed = TRUE))

  # Composite PK goes in the indexes block, not on individual columns
  expect_true(grepl('("user_id", "login_time") [pk]', result, fixed = TRUE))

  # Foreign key Ref statements (many-to-one direction)
  expect_true(grepl('Ref: "posts"."user_id" > "users"."id"',       result, fixed = TRUE))
  expect_true(grepl('Ref: "comments"."post_id" > "posts"."id"',    result, fixed = TRUE))
  expect_true(grepl('Ref: "login"."user_id" > "users"."id"',       result, fixed = TRUE))

  # Project block with database_type and note when project_name is supplied
  result_proj <- schema_dbml(conn, project_name = "mydb", note = "test note")
  expect_true(grepl('Project "mydb"',         result_proj, fixed = TRUE))
  expect_true(grepl('database_type: "SQLite"', result_proj, fixed = TRUE))
  expect_true(grepl("note: 'test note'",       result_proj, fixed = TRUE))
})
