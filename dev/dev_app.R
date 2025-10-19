## Example shiny app with rank list
# https://rstudio.github.io/sortable/reference/rank_list.html

# --- DUMMY EXAMPLE ---

# Foreign keys are a names list (table, keys)
fks <- list("a" = c("id"), "b" = c("id1", "id2"))

# dummy UI
ui <- fluidPage(
  mod_tableProp_UI("test"),
)

# dummy server with iris df
server <- function(input, output, session) {
  x <- mod_TableProp_server("test", iris, fks = fks)
  observe({
    test <<- x()
    print(x())
  })
}

shinyApp(ui, server)
