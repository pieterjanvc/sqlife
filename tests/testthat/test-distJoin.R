test_that("distJoin", {
  # Creat a schema with issues
  schemainfo <- list(
    tableInfo = data.frame(
      table = rep(1:3, each = 4),
      name = letters[1:12],
      pk = c(1, 1, 0, 0, 1, 1, rep(0, 6))
    ),
    foreignkeyInfo = data.frame(
      table = c(2, 2, 3, 3),
      from = c("g", "h", "k", "l"),
      to = c("l", "a", "a", "b"),
      fk_table = c(3, 1, 1, 1)
    )
  )

  # Check the key issues
  result <- keyCheck(schemainfo)

  # datapasta::df_paste(result$FKcheck)
  check <- list(
    statusCode = -3,
    PKcheck = data.frame(
      table = c(1L, 2L, 3L),
      hasPK = c(TRUE, TRUE, FALSE)
    ) |>
      tibble::as_tibble(),
    FKcheck = data.frame(
      stringsAsFactors = FALSE,
      table = c(2, 2, 2, 3, 3),
      from = c("h", NA, "g", "k", "l"),
      to = c("a", "b", "l", "a", "b"),
      fk_table = c(1, 1, 3, 1, 1),
      pk = c(1, 1, 0, 1, 1),
      reason = c("", "missingCompoundKey", "mapToNonPK", "", ""),
      issue = c(FALSE, TRUE, TRUE, FALSE, FALSE)
    )
  )

  expect_identical(result, check)
})
