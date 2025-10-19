## Example shiny app with rank list
# https://rstudio.github.io/sortable/reference/rank_list.html

# --- DUMMY EXAMPLE ---

# Foreign keys are a names list (table, keys)
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
