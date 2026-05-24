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


schemainfo <- schemaInfo(
  conn,
  include = tables[6]
)

schema_dbml(conn) |>
  schema_dbml_embed(show_in_browser = T)

schema_dbml(conn) |> cat()

tables <- c(
  "answer",
  "batch",
  "batch_review",
  "clerkship",
  "competency",
  "competency_diff",
  "competency_score",
  "competency_text",
  "evaluation",
  "evaluator",
  "prompt",
  "question",
  "review_assignment",
  "reviewer",
  "rotation",
  "rubric",
  "rubric_competency",
  "rubric_sentiment",
  "rubric_specificity",
  "rubric_utility",
  "sentiment",
  "specificity",
  "sqlite_sequence",
  "status_codes",
  "student",
  "utility"
)

schema_dbml(
  conn,
  keys_only = T
) |>
  schema_dbml_embed(show_in_browser = T) |>
  schema_dbml_iframe()

dbFinish(conn)


schema_dbml(
  conn,
  include = tables[4:5]
) |>
  cat()

dbml <- "Table clerkship {\n  id INTEGER [pk, increment]\n  clerkship TEXT [not null]\n  location TEXT\n}\n\nTable competency {\n  id INTEGER [pk, increment]\n  cID INTEGER [not null]\n  name TEXT [not null]\n  description TEXT [not null]\n  timestamp TEXT [default: `datetime('now', 'localtime')`]\n  note TEXT\n}\n\nTable competency_diff {\n  id INTEGER [pk, increment]\n  competency_id1 INTEGER [not null]\n  competency_id2 INTEGER\n  description TEXT [not null]\n  timestamp TEXT [default: `datetime('now', 'localtime')`]\n  note TEXT\n}\n\nRef: competency_diff.competency_id2 > competency.id [delete: cascade]"
schema_dbml_embed(dbml, show_in_browser = T)
# # SUCCESS

# Table clerkship {
#   id INTEGER [pk, increment]
#   clerkship TEXT [not null]
#   location TEXT
# }

# Table competency {
#   id INTEGER [pk, increment]
#   cID INTEGER [not null]
#   name TEXT [not null]
#   description TEXT [not null]
#   timestamp TEXT [default: `datetime('now', 'localtime')`]
#   note TEXT
# }

# Table competency_score {
#   id INTEGER [pk, increment]
#   review_assignment_id INTEGER [not null]
#   competency_id INTEGER [not null]
#   specificity INTEGER
#   note TEXT
# }

# Ref: competency_score.competency_id > competency.id [delete: cascade]

# #FAIL

# Table clerkship {
#   id INTEGER [pk, increment]
#   clerkship TEXT [not null]
#   location TEXT
# }

# Table competency {
#   id INTEGER [pk, increment]
#   cID INTEGER [not null]
#   name TEXT [not null]
#   description TEXT [not null]
#   timestamp TEXT [default: `datetime('now', 'localtime')`]
#   note TEXT
# }

# Table competency_diff {
#   id INTEGER [pk, increment]
#   competency_id1 INTEGER [not null]
#   competency_id2 INTEGER
#   description TEXT [not null]
#   timestamp TEXT [default: `datetime('now', 'localtime')`]
#   note TEXT
# }

# Ref: competency_diff.competency_id2 > competency.id [delete: cascade]
# Ref: competency_diff.competency_id1 > competency.id [delete: cascade]

#Other examples that FAIL
fail <- c(
  "Table competency {\n  id INTEGER [pk, increment]\n  cID INTEGER [not null]\n  name TEXT [not null]\n  description TEXT [not null]\n  timestamp TEXT [default: `datetime('now', 'localtime')`]\n  note TEXT\n}\n\nTable competency_score {\n  id INTEGER [pk, increment]\n  review_assignment_id INTEGER [not null]\n  competency_id INTEGER [not null]\n  specificity INTEGER\n  note TEXT\n}\n\nRef: competency_score.competency_id > competency.id [delete: cascade]",
  "Table competency {\n  id INTEGER [pk, increment]\n  cID INTEGER [not null]\n  name TEXT [not null]\n  description TEXT [not null]\n  timestamp TEXT [default: `datetime('now', 'localtime')`]\n  note TEXT\n}\n\nTable competency_diff {\n  id INTEGER [pk, increment]\n  competency_id1 INTEGER [not null]\n  competency_id2 INTEGER\n  description TEXT [not null]\n  timestamp TEXT [default: `datetime('now', 'localtime')`]\n  note TEXT\n}\n\nRef: competency_diff.competency_id2 > competency.id [delete: cascade]\nRef: competency_diff.competency_id1 > competency.id [delete: cascade]",
  "Table competency {\n  id INTEGER [pk, increment]\n  cID INTEGER [not null]\n  description TEXT [not null]\n    note TEXT\n}\n\nTable competency_diff {\n  id INTEGER [pk, increment]\n  competency_id1 INTEGER [not null]\n  competency_id2 INTEGER\n  description TEXT [not null]\n  \"timestamp\" TEXT [not null]\n}\n\nRef: competency_diff.competency_id2 > competency.id [delete: cascade]\nRef: competency_diff.competency_id1 > competency.id [delete: cascade]"
)

# Ones that work:
work <- c(
  "Table answer {\n  id INTEGER [pk, increment]\n  question_id INTEGER [not null]\n  evaluation_id INTEGER [not null]\n  submission_date TEXT [not null]\n  answer_txt TEXT\n  answer_txt_redacted TEXT\n  rowid INTEGER\n}\n\nTable evaluation {\n  id INTEGER [pk, increment]\n  rotation_id INTEGER [not null]\n  evaluator_id INTEGER [not null]\n  summary_flg INTEGER [not null]\n  complete INTEGER\n  acad_yr TEXT\n}\n\nRef: answer.evaluation_id > evaluation.id [delete: cascade]",
  "Table batch {\n  id INTEGER [pk, increment]\n  file_input_id TEXT\n  prompt_id INTEGER\n  batch_id TEXT\n  file_output_id TEXT\n  created TEXT [default: `datetime('now', 'localtime')`]\n  checked TEXT\n  finished TEXT\n  statusCode INTEGER [not null]\n  n_requests INTEGER\n  tokens_in INTEGER\n  tokens_out INTEGER\n  note TEXT\n}\n\nTable batch_review {\n  id INTEGER [pk, increment]\n  batch_id INTEGER [not null]\n  review_assignment_id INTEGER [not null]\n}\n\nRef: batch_review.batch_id > batch.id [delete: cascade]"
)

sapply(fail, function(x) {
  paste("DBML - Example\n\n", x, "\n\nENCODED\n\n", schema_dbml_embed(x))
}) |>
  paste(collapse = "\n\n-------------\n\n") |>
  cat()

schema_dbml_embed(work[1], show_in_browser = T)

cat(fail)


b64 <- base64enc::base64encode(charToRaw(fail[1]))
url <- paste0(
  "https://dbdiagram.io/embed?c=",
  URLencode(b64, reserved = F, repeated = T)
)
browseURL(url)

dbml <- "Table competency {\n  \"id\" INTEGER [pk, increment]\n  \"cID\" INTEGER [not null]\n  \"name\" TEXT [not null]\n  \"description\" TEXT [not null]\n  \"note\" TEXT\n}\n\nTable competency_diff {\n  \"id\" INTEGER [pk, increment]\n  \"competency_id1\" INTEGER [not null]\n  \"competency_id2\" INTEGER\n  \"description\" TEXT [not null]\n  \"timetamp\" TEXT [default: `datetime('now', 'localtime')`]\n  \"note\" TEXT\n}\n\nRef: competency_diff.\"competency_id2\" > competency.\"id\" [delete: cascade]\nRef: competency_diff.\"competency_id1\" > competency.\"id\" [delete: cascade]"
dbml <- "Table competency_diff {\n  id INTEGER [pk, increment]\n  competency_id1 INTEGER [not null]\n  competency_id2 INTEGER\n  description TEXT [not null]\n  timestamp TEXT [default: `datetime('now', 'localtime')`]\n}"
dbml <- paste0(
  "Table competency {\n  id INTEGER [pk, increment]\n  cID INTEGER [not null]\n  ",
  "description TEXT [not null]\n    note TEXT\n}\n\nTable competency_diff {\n  id INTEGER [pk, increment]\n  ",
  "competency_id1 INTEGER [not null]\n  competency_id2 INTEGER\n  description TEXT [not null]\n  ",
  "\"timestamp\" TEXT [not null]\n}\n\n",
  "Ref: competency_diff.competency_id2 > competency.id [delete: cascade]\nRef: competency_diff.competency_id1 > competency.id [delete: cascade]"
)
dbml <- paste0(
  "Table competency {\n  id INTEGER [pk, increment]\n  cID INTEGER [not null]\n  ",
  "description TEXT [not null]\n    note TEXT\n}\n\nTable competency_diff {\n  id INTEGER [pk, increment]\n  ",
  "competency_id1 INTEGER [not null]\n  competency_id2 INTEGER\n  description TEXT [not null]\n  ",
  "timestamp TEXT [not null]\n}\n\n",
  "Ref: competency_diff.competency_id2 > competency.id [delete: cascade]\nRef: competency_diff.competency_id1 > competency.id [delete: cascade]"
)
dbml <- paste0(
  "Table competency {\n  id INTEGER [pk, increment]\n  cID INTEGER [not null]\n  ",
  "description TEXT [not null]\n    note TEXT\n}\n\nTable competency_diff {\n  id INTEGER [pk, increment]\n  ",
  "competency_id1 INTEGER [not null]\n  competency_id2 INTEGER\n  description TEXT [not null]\n  ",
  "timestamp TEXT [not null]\n}\n\n"
)
dbml <- paste0(
  "Table competency {\n  \"id\" INTEGER [pk, increment]\n  cID INTEGER [not null]\n  ",
  "description TEXT [not null]\n    note TEXT\n}\n\nTable competency_diff {\n  id INTEGER [pk, increment]\n  ",
  "competency_id1 INTEGER [not null]\n  competency_id2 INTEGER\n  description TEXT [not null]\n  ",
  "\"timestamp\" TEXT [not null]\n}\n\n",
  "Ref: competency_diff.competency_id2 > competency.id [delete: cascade]\nRef: competency_diff.competency_id1 > competency.id [delete: cascade]"
)
dbml <- "Table competency {\n  id INTEGER [pk, increment]\n  cID INTEGER [not null]\n  name TEXT [not null]\n  description TEXT [not null]\n  \"timestamp\" TEXT [default: `datetime('now', 'localtime')`]\n}\n\nTable competency_diff {\n  id INTEGER [pk, increment]\n  competency_id1 INTEGER [not null]\n  competency_id2 INTEGER\n  description TEXT [not null]\n  \"timestamp\" TEXT [default: `datetime('now', 'localtime')`]\n  note TEXT\n}\n\nRef: competency_diff.competency_id2 > competency.id [delete: cascade]\nRef: competency_diff.competency_id1 > competency.id [delete: cascade]"
cat(dbml)
schema_dbml_embed(dbml, show_in_browser = T)
