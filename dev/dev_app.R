library(shiny)

dbInfo <- "../local/test.db"

ui <- fluidPage(
  actionButton("btn", "Click"),
  actionButton("btn2", "Click")
)

server <- function(input, output, session) {
  conn <- dbGetConn(dbInfo, session = session)

  # conn <- eventReactive(input$btn, {
  #   dbGetConn(dbInfo, session = session)
  # })

  # observeEvent(conn(), {
  #   tbl(conn(), "users") |> collect() |> print()
  # })

  observeEvent(input$btn, {
    x <- dbGetConn(conn)
    tbl(x, "users") |> collect() |> print()
    dbFinish(x)
  })

  observeEvent(input$btn2, {
    tbl(conn, "posts") |> collect() |> print()
  })
}

shinyApp(ui, server)
