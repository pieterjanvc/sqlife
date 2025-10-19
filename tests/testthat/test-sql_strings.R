test_that("word check", {
  words <- c("select", "test", "8test", "test8", "_test", "test OK", "TEST")

  result <- sql_wordCheck(words) |> as.data.frame()
  expected <- data.frame(
    word = c("select", "test", "8test", "test8", "_test", "test OK", "TEST"),
    reserved = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    needsQuotes = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
    unique = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE),
    suggested = c(
      "select",
      "test",
      "x8test",
      "test8",
      "test_2",
      "test_ok",
      "test_3"
    )
  )
  expect_identical(result, expected)
})

test_that("create SQL INSERT statement", {})
