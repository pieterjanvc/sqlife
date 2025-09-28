test_that("Check data manipulation functions", {
  path <- tempfile(fileext = ".db")
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

  dbFinish(conn)

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

  file.remove(path)

  # shell.exec(path)
  # shell.exec(dirname(path))
})
