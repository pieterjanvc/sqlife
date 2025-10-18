dbInfo <- "local/test.db"
schema <- "tests/testthat/testdata/dummy1.sql"
dbSetup(dbInfo, schema, validateSchema = T)


colabNetDB <- "D:/Desktop/testCN.db"
schema <- system.file("create_colabNetDB.sql", package = "colabNet")

sqlife::dbSetup(colabNetDB, schema = schema)

sqlife::dbNewFromSchema(colabNetDB, schema = schema)
