test_that("Test columnn check", {
  schema <- test_path("testdata", "dummy1.sql")
  conn <- dbNewFromSchema(schema = schema, memory = ":memory:")$conn

  df <- data.frame(
    id = c(4L, 5L),
    username = c("user1", "user2"),
    email = c("user1@gmail.com", "user2@gmail.com")
  )

  result <- dbColumnCheck(df, conn, "users")

  # Perfect
  expected <- list(
    success = TRUE,
    statusCodes = 1,
    pk = "id",
    missingPK = character(0),
    uniqueRows = TRUE,
    notInTable = NULL,
    missingNotNull = NULL
  )
  expect_identical(result[!names(result) %in% c("msg")], expected)

  # Errors
  df <- data.frame(
    email = c("user1@gmail.com", "user2@gmail.com"),
    test = c(1, 2)
  )

  result <- dbColumnCheck(df, conn, "users")

  expected <- list(
    success = F,
    statusCodes = c(-2, -3, -4, -5),
    pk = "id",
    missingPK = "id",
    uniqueRows = FALSE,
    notInTable = "test",
    missingNotNull = "username"
  )
  expect_identical(result[!names(result) %in% c("msg")], expected)

  # Allow NOT NULL columns missing
  result <- dbColumnCheck(df, conn, "users", notNUllError = F)
  expected <- list(
    success = F,
    statusCodes = c(-2, -3, -4, 2),
    pk = "id",
    missingPK = "id",
    uniqueRows = FALSE,
    notInTable = "test",
    missingNotNull = "username"
  )
  expect_identical(result[!names(result) %in% c("msg")], expected)

  #Clean up
  dbFinish(conn, showWarnings = F)
})
