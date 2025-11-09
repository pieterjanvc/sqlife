library(shiny)

dbInfo <- "../local/test.db"

ui <- fluidPage(
  actionButton("btn", "Click"),
  mod_dbSetup_ui("db")
)

server <- function(input, output, session) {
  connInfo <- mod_dbSetup_server("db", useDB = dbInfo)
  # connInfo <- mod_dbSetup_server(
  #   "db",
  #   localFolder = "../local",
  #   tempFolder = "../local/temp",
  #   schema = "../tests/testthat/testdata/dummy1.sql"
  # )
  observe({
    print(connInfo())
  })
}

shinyApp(ui, server)
