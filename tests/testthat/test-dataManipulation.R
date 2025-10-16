test_that("Check data manipulation functions", {
  path <- tempfile(fileext = ".db")
  on.exit(file.remove(path))
  schema <- test_path("testdata", "dummy1.sql")
  result <- dbSetup(path, schema, validateSchema = T)

  dataframe <- data.frame(
    username = c("user1", "user2"),
    email = c("user1@gmail.com", "user2@gmail.com")
  )

  # Insert from path with commit
  result <- tbl_insert(dataframe, path, "users")

  expected <- data.frame(
    stringsAsFactors = FALSE,
    id = c(4L, 5L),
    username = c("user1", "user2"),
    email = c("user1@gmail.com", "user2@gmail.com")
  )

  expect_identical(result, expected)

  # Insert from path without commit
  expect_error(tbl_insert(dataframe, path, "users", commit = F))

  # Insert from connection without commit
  conn <- dbGetConn(path)
  result <- tbl_insert(dataframe, conn, "users", commit = F)

  expected <- data.frame(
    stringsAsFactors = FALSE,
    id = c(6L, 7L),
    username = c("user1", "user2"),
    email = c("user1@gmail.com", "user2@gmail.com")
  )

  expect_true(sqliteIsTransacting(conn))
  expect_identical(result, expected)

  info <- dbFinish(conn, closeExisting = T)
  expect_identical(info, list(changed = T, transacting = F, closed = T))

  # Allow missing columns when not required
  dataframe <- data.frame(
    user_id = 1,
    login_time = "now"
  )

  result <- tbl_insert(dataframe, path, "login")

  expected <- data.frame(
    stringsAsFactors = FALSE,
    user_id = c(1),
    login_time = c("now"),
    info = c(NA_character_)
  )
  expect_identical(result, expected)

  # Error in not NULL missing
  dataframe <- data.frame(
    post_id = c(1, 1),
    comment_text = c("Cool", NA)
  )

  expect_error(tbl_insert(dataframe, path, "comments"))

  # --- Update data
  dataframe <- data.frame(
    id = c(1L, 3L),
    username = c("person1", "person2")
  )

  result <- tbl_update(dataframe, path, "users")

  expected <- data.frame(
    stringsAsFactors = FALSE,
    id = c(1L, 3L),
    username = c("person1", "person2")
  )

  expect_identical(result, expected)

  # Missing primary key
  expect_error(tbl_update(dataframe |> select(-id), path, "users"))

  # Non-existing columns
  expect_error(tbl_update(dataframe |> mutate(x = 5), path, "users"))

  # --- Delete data
  dataframe <- data.frame(
    id = c(1L, 3L),
    post_id = c(1L, 3L)
  )

  conn <- dbNewFromSchema(schema = schema, memory = ":memory:")$conn

  result <- tbl_delete(dataframe, conn, "comments")

  expected <- data.frame(
    id = c(1L, 3L),
    post_id = c(1L, 3L),
    comment_text = c(
      "Nice post, Alice!",
      "Haha; I laughed at the semicolon usage. Keep it up!"
    )
  )

  expect_identical(result, expected)

  . <- dbFinish(conn, closeExisting = T, showWarnings = F)
})
