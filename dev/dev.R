conn <- dbGetConn("../CFME/local/cfme.db")
conn <- dbGetConn("local/test.db")

dbInfo <- "local/test.db"
schema <- "tests/testthat/testdata/dummy1.sql"
dbSetup(dbInfo, schema, validateSchema = T)

schemainfo <- schemaInfo(conn)

# check <- keyCheck(schemainfo)
# if (check$statusCode < 0) {
#   stop(
#     "The schema has the following issues\nPrimary Keys\n",
#     dfAsText(check$PKcheck |> filter(!hasPK)),
#     "\n\nForeign Keys\n",
#     dfAsText(check$FKcheck |> filter(issue))
#   )
# }
addSelect = T
conn <- dbGetConn("../CFME/local/cfme.db")
toJoin <- c("answer", "clerkship")
toJoin <- c("clerkship", "question")
dist_join(conn, toJoin)


conn <- dbGetConn("local/test.db")
toJoin <- c("users", "login")
dist_join(conn, toJoin)

file.remove("local/temp.db")
dbSetup("local/temp.db", "inst/example.sql")
conn <- dbGetConn("local/temp.db")
toJoin <- c("users", "login")
dist_join(conn, toJoin)

dbFinish(conn)
