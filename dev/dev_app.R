library(shiny)

dbInfo <- "../local/test.db"

ui <- fluidPage(
  actionButton("btn", "Click"),
  mod_dbSetup_ui("db")
)

server <- function(input, output, session) {
  connInfo <- mod_dbSetup_server("db", useDB = dbInfo)
}

shinyApp(ui, server)
