## Example shiny app with rank list
# https://rstudio.github.io/sortable/reference/rank_list.html

library(shiny)
library(sortable)

dfToSettings <- function(dataframe, addNew = 0) {
  data.frame(
    index = 1:(ncol(dataframe) + addNew),
    name = c(colnames(dataframe), rep("", addNew)),
    pk = F,
    nn = F,
    type = "INTEGER",
    fktable = "",
    fkid = "",
    odc = F
  )
}

mod_colProp_UI <- function(id, name, pk, nn, type, fktable, fkid, odc, fks) {
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
        tags$td(textInput(ns("name"), label = NULL, value = name)),
        tags$td(
          checkboxInput(ns("pk"), label = NULL, value = pk),
          style = "text-align: center;"
        ),
        tags$td(
          checkboxInput(ns("nn"), label = NULL, value = nn),
          style = "text-align: center;"
        ),
        tags$td(
          selectInput(
            ns("type"),
            label = NULL,
            choices = c("INTEGER", "REAL", "TEXT", "BLOB"),
            selected = type
          )
        ),
        tags$td(selectInput(
          ns("fktable"),
          label = NULL,
          choices = c("none" = "", names(fks)),
          selected = fktable,
          selectize = F
        )),
        tags$td(selectInput(
          ns("fkid"),
          label = NULL,
          choices = c("none" = "", unlist(fks[fktable])),
          selected = fkid,
          selectize = F
        )),
        tags$td(
          checkboxInput(ns("odc"), label = NULL, value = odc),
          style = "text-align: center;"
        )
      )
    )
  )
}

mod_colProp_server <- function(id, fks) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$fktable, {
      # Check if a table is selected or not
      if (is.null(fks[[input$fktable]])) {
        choices = c("none" = "")
      } else {
        choices = fks[[input$fktable]]
      }

      updateSelectInput(session, "fkid", choices = choices)
    })

    # return all values
    return(input)
  })
}

# id = ns(as.character(settings$index[i]))
# name = settings$name[i]
# pk = settings$pk[i]
# nn = settings$nn[i]
# type = settings$type[i]
# fktable = settings$fktable[i]
# fkid = settings$fkid[i]
# odc = settings$odc[i]

generateSettingsUI <- function(settings, id) {
  ns <- NS(id)
  labels <- setNames(
    lapply(1:nrow(settings), function(i) {
      div(mod_colProp_UI(
        id = ns(as.character(settings$index[i])),
        name = settings$name[i],
        pk = settings$pk[i],
        nn = settings$nn[i],
        type = settings$type[i],
        fktable = settings$fktable[i],
        fkid = settings$fkid[i],
        odc = settings$odc[i],
        fks = fks
      ))
    }),
    as.character(1:nrow(settings))
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
    actionButton(ns("save"), "Save"),
    actionButton(ns("add"), "Add attribute"),
    actionButton(ns("reset"), "Reset"),
    rank_list(
      text = "You can reorder the columns by dragging them ...",
      labels = labels,
      input_id = ns("rank")
    )
  )
}

mod_tableProp_UI <- function(id) {
  uiOutput(NS(id, "settingsTable"))
}


mod_TableProp_server <- function(id, dataframe, fks) {
  settings <- dfToSettings(iris)

  outToDF <- function(out, rank) {
    new <- lapply(out, reactiveValuesToList)
    new <- do.call(rbind, lapply(new, as.data.frame))
    new$index <- 1:nrow(new)

    if (length(rank) > 0) {
      new <- new[rank |> as.integer(), ]
    }

    new
  }

  moduleServer(id, function(input, output, session) {
    settingsUI <- reactiveVal(generateSettingsUI(settings, id))

    output$settingsTable <- renderUI({
      settingsUI()
    })

    out <- reactiveVal({
      lapply(1:ncol(dataframe), function(i) {
        mod_colProp_server(as.character(i), fks = fks)
      })
    })

    # Needed to trigger the fkid updates
    observe({
      out()
    })

    observeEvent(input$add, {
      newSettings <- bind_rows(
        dfToSettings(data.frame(), 1),
        outToDF(out(), input$rank) |> mutate(index = index + 1)
      )

      out({
        lapply(1:nrow(newSettings), function(i) {
          mod_colProp_server(as.character(i), fks = fks)
        })
      })

      settingsUI(generateSettingsUI(newSettings, id))
    })

    observeEvent(input$reset, {
      settingsUI(generateSettingsUI(settings, id))
    })

    df <- eventReactive(input$save, {
      outToDF(out(), input$rank)
    })

    return(df)
  })
}


fks <- list("a" = c("id"), "b" = c("id1", "id2"))

ui <- fluidPage(
  mod_tableProp_UI("test"),
)

server <- function(input, output, session) {
  x <- mod_TableProp_server("test", iris, fks = fks)
  observe({
    print(x())
  })
}

shinyApp(ui, server)
