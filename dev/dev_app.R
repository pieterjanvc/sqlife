## Example shiny app with rank list
# https://rstudio.github.io/sortable/reference/rank_list.html

# --- DUMMY EXAMPLE ---

dbPKlist <- function(dbInfo, schema) {
  if (missing(schema)) {
    conn <- dbGetConn(dbInfo)
  } else {
    conn <- dbNewFromSchema(
      schema = schema,
      memory = ":memory:",
      returnConn = T
    )$conn
  }
  on.exit(dbFinish(conn, commit = F))
  tables <- dbListTables(conn)
  tables <- tables[!tables %in% "sqlite_sequence"]
  setNames(
    lapply(tables, function(table) {
      dbGetQuery(conn, sprintf("PRAGMA table_info('%s');", table)) |>
        filter(pk == 1) |>
        pull(name)
    }),
    tables
  )
}

# Foreign keys are a names list (table, keys)
#schema = "tests/testthat/testdata/dummy1.sql"
pks <- dbPKlist(schema = "../tests/testthat/testdata/dummy1.sql")

# dummy UI
ui <- fluidPage(
  mod_tableProp_UI("test"),
)

# dummy server with iris df
server <- function(input, output, session) {
  x <- mod_TableProp_server("test", iris, pks = pks)
  observe({
    test <<- x()
    print(x())
  })
}

shinyApp(ui, server)
