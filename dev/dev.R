dbInfo <- "local/test.db"
schema <- "tests/testthat/testdata/dummy1.sql"
dbSetup(dbInfo, schema, validateSchema = T)

conn <- dbGetConn(dbInfo)
attr(conn, "existing")

conn2 <- dbGetConn(dbInfo)
attr(conn2, "existing")

dataframe <- data.frame(
  stringsAsFactors = FALSE,
  id = c(6L, 7L),
  username = c("user1", "user2"),
  email = c("user1@gmail.com", "user2@gmail.com")
)
