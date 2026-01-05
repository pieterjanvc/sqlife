test_that("Schema checks", {
  toCheck <- data.frame(
    table = c("users", "users", "users", "login", "comments"),
    attr = c("id", "e-mail", "DOB", "times", "id"),
    new = c(F, F, T, F, T)
  )

  check_names_attr(conn, toCheck, error = F)
  check_names_attr(conn, toCheck |> slice(1, 3, 4), error = F)
})
