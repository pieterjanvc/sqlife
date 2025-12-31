test_that("Shiny modules", {
  message("Waiting for https://github.com/rstudio/shinytest2/issues/431")
  # library(shinytest2)
  #
  # # Create a temp database
  # schema <- test_path("testdata", "dummy1.sql") |> normalizePath()
  # fol <- tempdir()
  # path <- tempfile(fileext = ".db", tmpdir = fol)
  # # print(normalizePath(fol, winslash = "/"))
  # on.exit({
  #   file.remove(path)
  #   unlink(fol, recursive = T)
  # })
  # result <- dbSetup(path, schema, validateSchema = T)
  #
  # # Create a dummy app with the module
  # testApp <- shinyApp(
  #   ui <- fluidPage(
  #     mod_dbSetup_ui("test")
  #   ),
  #
  #   server <- function(input, output, session) {
  #     connInfo <- mod_dbSetup_server(
  #       id = "test",
  #       localFolder = fol,
  #       tempFolder = fol,
  #       schema = schema
  #     )
  #   }
  # )
  #
  # # Run the app, create new database and download it
  # app <- AppDriver$new(app = testApp)
  # app$set_inputs("test-option" = "4")
  # app$set_inputs("test-db_new" = "test")
  # app$click("test-start")
  # suppressWarnings(downl <- app$get_download("test-dbDownload"))
  # expect_no_error(app$get_values())
  # app$stop()
  #
  # # Check the downloaded database
  # conn <- dbGetConn(downl)
  # expect_true(tbl(conn, "users") |> collect() |> nrow() == 3)
  # dbFinish(conn)
})
