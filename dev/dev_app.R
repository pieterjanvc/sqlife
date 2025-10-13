library(shiny)

ui <- fluidPage(
  mod_dbSetup_ui("test")
)

server <- function(input, output, session) {
  mod_dbSetup_server(
    "test",
    localFolder = "D:/Desktop/",
    tempFolder = "../local/temp",
    schema = "../tests/testthat/testdata/dummy1.sql"
  )
}

shinyApp(ui, server)
