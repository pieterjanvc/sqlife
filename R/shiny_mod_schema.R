## Example shiny app with rank list
# https://rstudio.github.io/sortable/reference/rank_list.html

library(shiny)
library(sortable)


#' Row in the table with setting for a specific attribute
#'
#' @param id Module ID
#' @param sel T/F Is the row selected
#' @param name Name of the table attribute / column
#' @param pk T/F is primary key
#' @param nn T/F is NOT NULL
#' @param type SQLite data type : INTEGER, REAL, TEXT or BLOB
#' @param default Default value
#' @param fktable Name of the foreign key table for this attribute
#' @param fkid Name of the foreign key id for this attribute
#' @param odc T/F ON DELETE CASCADE in case of foreign key
#' @param pks Names list of foreign keys (names are tables)
#'
#' @import shiny
#'
#' @returns A row of inputs organised as an HTML table for correct spacing
#' @export
#'
mod_colProp_UI <- function(
  id,
  sel,
  name,
  pk,
  nn,
  type,
  default,
  fktable,
  fkid,
  odc,
  pks
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
      tags$col(style = "width: 10%"),
      tags$col(style = "width: 15%"),
      tags$col(style = "width: 20%"),
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
        tags$td(textInput(ns("default"), label = NULL, value = default)),
        tags$td(selectInput(
          ns("fktable"),
          label = NULL,
          choices = c("none" = "", names(pks)),
          selected = fktable,
          selectize = F
        )),
        tags$td(selectInput(
          ns("fkid"),
          label = NULL,
          choices = c("none" = "", unlist(pks[fktable])),
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


#' Server for a specific attribute's settings
#'
#' @param id Module ID
#' @param pks Named list of primary keys (names are tables)
#'
#' @import shiny
#'
#' @returns All inputs as a reactiveValues
#' @export
#'
mod_colProp_server <- function(id, pks) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$fktable, {
      # Check if a table is selected or not
      if (is.null(pks[[input$fktable]])) {
        choices = c("none" = "")
      } else {
        choices = pks[[input$fktable]]
      }

      updateSelectInput(session, "fkid", choices = choices)
    })

    # return all values
    return(input)
  })
}


#' Module for the table UI
#'
#' @param id Module ID
#'
#' @import shiny
#'
#' @returns UI for the table properties with a sortable list
#' @export
#'
mod_tableProp_UI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("name"), "Table name"),

    # Buttons above the table
    actionButton(ns("generate"), "Generate"),
    actionButton(ns("add"), "Add attribute"),
    actionButton(ns("del"), "Remove selected"),
    actionButton(ns("reset"), "Reset"),
    tags$span(
      checkboxInput(ns("autoQuote"), "Auto quote strings", value = T),
      style = "display: inline-block; margin-left: 10px;"
    ),
    uiOutput(ns("statement")),
    uiOutput(ns("settingsTable"))
  )
}


#' Module for the table server
#'
#' @param id Module ID
#' @param dataframe (Optional) datafame to start from. If not set, default table
#' with a single attribute / column is generated
#' @param pks Named list of primary keys (names are tables)
#'
#' @import shiny sortable dplyr highlighter
#'
#' @returns A dataframe with table attribute properties that can be used to
#' generate a CREATE statement
#' @export
#'
mod_TableProp_server <- function(id, dataframe, pks) {
  # --- FUNCTIONS ---

  # Convert a dataframe to table creation settings
  dfToSettings <- function(dataframe, addNew = 0) {
    if (missing(dataframe)) {
      dataframe <- data.frame()
    }

    data.frame(
      index = 1:(ncol(dataframe) + addNew),
      name = c(colnames(dataframe), rep("", addNew)),
      pk = F,
      nn = F,
      type = "INTEGER",
      default = "",
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
          default = settings$default[i],
          fktable = settings$fktable[i],
          fkid = settings$fkid[i],
          odc = settings$odc[i],
          pks = pks
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

      # These are the table headers
      tags$table(
        style = "width: 95%; margin-left: 20px; text-align: center;",
        tags$col(style = "width: 5%"),
        tags$col(style = "width: 20%"),
        tags$col(style = "width: 5%"),
        tags$col(style = "width: 5%"),
        tags$col(style = "width: 15%"),
        tags$col(style = "width: 10%"),
        tags$col(style = "width: 15%"),
        tags$col(style = "width: 20%"),
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
          tags$td(div(tags$b("def"), title = "Default value (optional)")),
          tags$td(div(tags$b("FK table"), title = "Foreign key table")),
          tags$td(div(tags$b("FK name"), title = "Foreign key name")),
          tags$td(div(
            tags$b("ODC"),
            title = " Foreign key has ON DELETE CASCADE"
          )) # ON DELTETE CASCASE
        )
      ),

      # Ranked list allows reordering
      # https://rstudio.github.io/sortable/reference/rank_list.html
      rank_list(
        text = NULL,
        labels = labels,
        input_id = ns("rank")
      )
    )
  }

  # Use the variable name of the passed data frame if provided
  defaultName <- ifelse(missing(dataframe), "", deparse(substitute(dataframe)))

  settings <- dfToSettings(dataframe, addNew = ifelse(missing(dataframe), 1, 0))

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
    statementUI <- reactiveVal()

    updateTextInput(session, "name", value = defaultName)

    # Dynamic UI with attribute settings table
    output$settingsTable <- renderUI({
      settingsUI()
    })

    # Reactive variable to capture the sub-modules' outputs
    out <- reactiveVal({
      lapply(1:nrow(settings), function(i) {
        # Sub-module for a specific attribute
        mod_colProp_server(as.character(i), pks = pks)
      })
    })

    # Add a new attribute to the table
    observeEvent(input$add, {
      newSettings <- bind_rows(
        dfToSettings(data.frame(), 1),
        outToDF(out(), input$rank) |> mutate(index = index + 1)
      )

      out({
        lapply(newSettings$index, function(i) {
          mod_colProp_server(as.character(i), pks = pks)
        })
      })

      settingsUI(generateSettingsUI(newSettings, id))
      statementUI("")
    })

    # Delete selected attributes from the table
    observeEvent(input$del, {
      newSettings <- outToDF(out(), input$rank) |> filter(!sel)

      out({
        lapply(newSettings$index, function(i) {
          mod_colProp_server(as.character(i), pks = pks)
        })
      })

      settingsUI(generateSettingsUI(newSettings, id))
      statementUI("")
    })

    # Reset the table to original state
    observeEvent(input$reset, {
      settingsUI(generateSettingsUI(settings, id))
      statementUI("")
    })

    # The dataframe with all settings to return
    df <- eventReactive(input$generate, {
      outToDF(out(), input$rank)
    })

    observeEvent(df(), {
      x <- sql_create(
        df(),
        tableName = input$name,
        pks = pks,
        autoQuote = input$autoQuote
      )

      statementUI(tagList(
        if (any(x$statusCode < 0)) {
          div(
            tags$h3("Errors"),
            tags$ul(
              lapply(x$msg[x$statusCode < 0], function(item) {
                tags$li(item)
              })
            ),
            style = "color:red;"
          )
        },
        if (any(x$statusCode > 1)) {
          div(
            tags$h3("Notes"),
            tags$ul(
              lapply(x$msg[x$statusCode > 0], function(item) {
                tags$li(item)
              })
            )
          )
        },
        highlighter(x$statement, language = "sql")
      ))
    })

    output$statement <- renderUI({
      statementUI()
    })

    return(df)
  })
}
