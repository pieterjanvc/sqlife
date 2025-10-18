## Example shiny app with rank list
# https://rstudio.github.io/sortable/reference/rank_list.html

library(shiny)
library(sortable)

colSettings <- function(id) {}

mod_colProp_UI <- function(id, colname, fktables) {
  ns <- NS(id)
  tagList(
    tags$table(
      style = "width: 100%; table-layout: fixed;",
      tags$col(style = "width: 20%"),
      tags$col(style = "width: 5%"),
      tags$col(style = "width: 5%"),
      tags$col(style = "width: 15%"),
      tags$col(style = "width: 25%"),
      tags$col(style = "width: 25%"),
      tags$col(style = "width: 5%"),
      tags$tr(
        tags$td(tags$p(colname)),
        tags$td(
          checkboxInput(ns("pk"), label = NULL),
          style = "text-align: center;"
        ),
        tags$td(
          checkboxInput(ns("nn"), label = NULL),
          style = "text-align: center;"
        ),
        tags$td(
          selectInput(
            ns("type"),
            label = NULL,
            choices = c("INTEGER", "REAL", "TEXT", "BLOB")
          )
        ),
        tags$td(selectInput(
          ns("fktable"),
          label = NULL,
          choices = c("none" = "", fktables),
          selectize = F
        )),
        tags$td(selectInput(
          ns("fkid"),
          label = NULL,
          choices = c("none" = ""),
          selectize = F
        )),
        tags$td(
          checkboxInput(ns("odc"), label = NULL),
          style = "text-align: center;"
        )
      )
    )
  )
}

mod_colProp_server <- function(id, fkids) {
  moduleServer(id, function(input, output, session) {
    observeEvent(
      input$fktable,
      {
        # Check if a table is selected or not
        if (is.null(fkids[[input$fktable]])) {
          choices = c("none" = "")
        } else {
          choices = fkids[[input$fktable]]
        }

        updateSelectInput(session, "fkid", choices = choices)
      }
    )

    # return all values
    return(input)
  })
}

mod_tableProp_UI <- function(id, dataframe, fktables) {
  ns <- NS(id)
  labels <- setNames(
    lapply(1:ncol(dataframe), function(i) {
      cname <- colnames(dataframe)[i]
      fktables = fktables
      div(mod_colProp_UI(
        ns(as.character(i)),
        colname = cname,
        fktables = fktables
      ))
    }),
    as.character(1:ncol(dataframe))
  )
  tagList(
    tags$head(
      tags$style(HTML(
        "
      /* Allow overflow of selectInput dropdowns */
      .rank-list {
        overflow: visible !important;
      }
      .rank-list-item {
        overflow: visible !important;
      }
    "
      ))
    ),
    rank_list(
      text = "You can rearrange column order by dragging them",
      labels = labels,
      input_id = ns("rank")
    )
  )
}


mod_TableProp_server <- function(id, dataframe, fkids) {
  moduleServer(id, function(input, output, session) {
    out <- reactive({
      lapply(1:ncol(dataframe), function(i) {
        mod_colProp_server(as.character(i), fkids = fkids)
      })
    })

    df <- reactive({
      new <- lapply(out(), reactiveValuesToList)
      new <- do.call(rbind, lapply(new, as.data.frame))
      new$oldOrder <- 1:ncol(dataframe)

      if (length(input$rank) > 0) {
        new <- new[input$rank |> as.integer(), ]
      }

      new$newOrder <- 1:ncol(dataframe)
      new
    })

    return(df)
  })
}

ui <- fluidPage(
  mod_tableProp_UI("test", iris, fktables = c("a", "b"))
)

server <- function(input, output, session) {
  x <- mod_TableProp_server("test", iris, list("b" = c("id")))
  observe({
    print(x())
  })
}

shinyApp(ui, server)
