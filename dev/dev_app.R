## Example shiny app with rank list
# https://rstudio.github.io/sortable/reference/rank_list.html

library(shiny)
library(sortable)

# Row in the table with setting for a specific attribute
mod_colProp_UI <- function(
  id,
  sel,
  name,
  pk,
  nn,
  type,
  fktable,
  fkid,
  odc,
  fks
) {
  ns <- NS(id)

  tagList(
    tags$table(
      style = "width: 100%; table-layout: fixed;",
      tags$col(style = "width: 5%"),
      tags$col(style = "width: 20%"),
      tags$col(style = "width: 5%"),
      tags$col(style = "width: 5%"),
      tags$col(style = "width: 15%"),
      tags$col(style = "width: 20%"),
      tags$col(style = "width: 25%"),
      tags$col(style = "width: 5%"),
      tags$tr(
        tags$td(
          checkboxInput(ns("sel"), label = NULL, value = sel),
          style = "text-align: center;"
        ),
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

# Server for a specific attribute's settings
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

# Module for the table UI
mod_tableProp_UI <- function(id) {
  uiOutput(NS(id, "settingsTable"))
}

# Module for the table server
mod_TableProp_server <- function(id, dataframe, fks) {
  # --- FUNCTIONS ---

  # Convert a dataframe to table creation settings
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

  # Function to generate all attribute UI settings for a table
  generateSettingsUI <- function(settings, id) {
    ns <- NS(id)
    labels <- setNames(
      lapply(1:nrow(settings), function(i) {
        div(mod_colProp_UI(
          id = ns(as.character(settings$index[i])),
          sel = F,
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
      # Make sure that dropdowns do not get clipped
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
      textInput("name", "Table name"),

      # Buttons above the table
      actionButton(ns("save"), "Save"),
      actionButton(ns("add"), "Add attribute"),
      actionButton(ns("del"), "Remove selected"),
      actionButton(ns("reset"), "Reset"),

      # These are the table headers
      tags$table(
        style = "width: 95%; margin-left: 20px; text-align: center;",
        tags$col(style = "width: 5%"),
        tags$col(style = "width: 20%"),
        tags$col(style = "width: 5%"),
        tags$col(style = "width: 5%"),
        tags$col(style = "width: 15%"),
        tags$col(style = "width: 20%"),
        tags$col(style = "width: 25%"),
        tags$col(style = "width: 5%"),
        tags$tr(
          tags$td(div(tags$b("sel"), title = "Select attributes")),
          tags$td(div(
            tags$b("attribute"),
            title = "Attribute / column name for the table"
          )),
          tags$td(div(tags$b("PK"), title = "Part of the primary key")),
          tags$td(div(tags$b("NN"), title = "Values NOT NULL")), # NOT NULL
          tags$td(div(tags$b("type"), title = "SQLIte data type")),
          tags$td(div(tags$b("FK table"), title = "Foreign key table")),
          tags$td(div(tags$b("FK name"), title = "Foreign key name")),
          tags$td(div(
            tags$b("ODC"),
            title = " Foreign key has ON DELETE CASCADE"
          )) # ON DELTETE CASCASE
        )
      ),

      # Ranked list allows reordering
      rank_list(
        text = NULL,
        labels = labels,
        input_id = ns("rank")
      )
    )
  }

  settings <- dfToSettings(dataframe)

  # Convert a reactive list of row settings into a data frame
  outToDF <- function(out, rank) {
    new <- lapply(out, reactiveValuesToList)
    new <- do.call(rbind, lapply(new, as.data.frame))
    new$index <- 1:nrow(new)

    if (length(rank) > 0) {
      new <- new[rank |> as.integer(), ]
    }

    new
  }

  # --- SERVER ---

  moduleServer(id, function(input, output, session) {
    settingsUI <- reactiveVal(generateSettingsUI(settings, id))

    # Dynamic UI with attribute settings table
    output$settingsTable <- renderUI({
      settingsUI()
    })

    # Reactive variable to capture the sub-modules' outputs
    out <- reactiveVal({
      lapply(1:ncol(dataframe), function(i) {
        # Sub-module for a specific attribute
        mod_colProp_server(as.character(i), fks = fks)
      })
    })

    #     # Needed to trigger the fkid updates
    #     observe({
    #       out()
    #     })

    # Add a new attribute to the table
    observeEvent(input$add, {
      newSettings <- bind_rows(
        dfToSettings(data.frame(), 1),
        outToDF(out(), input$rank) |> mutate(index = index + 1)
      )

      out({
        lapply(newSettings$index, function(i) {
          mod_colProp_server(as.character(i), fks = fks)
        })
      })

      settingsUI(generateSettingsUI(newSettings, id))
    })

    # Delete selected attributes from the table
    observeEvent(input$del, {
      newSettings <- outToDF(out(), input$rank) |> filter(!sel)

      out({
        lapply(newSettings$index, function(i) {
          mod_colProp_server(as.character(i), fks = fks)
        })
      })

      settingsUI(generateSettingsUI(newSettings, id))
    })

    # Reset the table to original state
    observeEvent(input$reset, {
      settingsUI(generateSettingsUI(settings, id))
    })

    # The dataframe with all settings to return
    df <- eventReactive(input$save, {
      outToDF(out(), input$rank)
    })

    return(df)
  })
}

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
    print(x())
  })
}

shinyApp(ui, server)
