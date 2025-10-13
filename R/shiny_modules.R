# showOpenFilePicker() would be great but not universally supported
#' Decorate an HTML element with a message
#'
#' @param elementID The ID of the element as set in Shiny
#' @param message (Optional) If set, this message will be shown, if not any
#' existing message for this ID will be removed
#' @param type (Default = "error"). info, error or success (will define the colour)
#' @param where (Default = "afterEnd) Relative position of the message to the
#' ID. Any of "beforeStart", "afterStart", "beforeEnd" or "afterEnd"
#' @param session (Default = getDefaultReactiveDomain()). Shiny session object
#'
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom shiny insertUI removeUI getDefaultReactiveDomain
#'
#' @return A dataset that can be used to create a (plotly) Treemap
#' @export
#'
elementMsg <- function(
  elementID,
  message,
  type = "error",
  where = "afterEnd",
  session = getDefaultReactiveDomain()
) {
  # Make sure the tag ID starts with # or .
  tagID <- ifelse(
    str_detect(elementID, "^[.#]"),
    elementID,
    paste0("#", elementID)
  )

  # Remove existing element
  removeUI(paste0(tagID, "_msg"), session = session)

  # Stop if no new message
  if (missing(message)) {
    return()
  }

  # Add / update message
  msg <- tags$div(
    tags$i(
      message,
      style = sprintf(
        "color:%s",
        case_when(
          type == "info" ~ "#2196f3",
          type == "error" ~ "#f44336",
          type == "success" ~ "#4caf50",
          TRUE ~ "#262626"
        )
      )
    ),
    id = paste0(elementID, "_msg")
  )

  insertUI(selector = tagID, where = where, msg, session = session)
}


#' Download button for the database
#'
#'
#' @param id ID must match mod_dbSetup_server
#' @param download (Default = "button"). Button or link
#'
#' @returns Shiny UI as tagList
#' @export
#'
mod_dbSetup_ui <- function(id, download = "button") {
  tagList(
    if (download == "button") {
      downloadButton(NS(id, "dbDownload"), "Download database")
    } else if (download == "link") {
      downloadLink(NS(id, "dbDownload"), "Download database")
    } else if (download == "none") {
      NULL
    }
  )
}

#' Module server to setup an SQLite database in various ways
#'
#'
#' @param id ID name for the server
#' @param localFolder Folder with existing, permanent databases to provide
#' @param tempFolder Folder where temp (new and uploaded) databases live
#' @param schema The schema of the SQLite database (.sql file)
#' @param options (Default = c(1,2,3,4)) Cector with integers indicating which DB
#' options will be available.
#'  1 - Explore a database on the server
#'  2 - Upload a database from your computer
#'  3 - Resume with a previously uploaded database
#'  4 - Start a new database
#' @param useDB Default = NULL. If set, the provided database is used and the
#' rest is skipped. This is especially useful for dev when you don't want the
#' modal pop-up. If there is not DB at the specified path, a new one is created
#'
#' @import shiny
#' @importFrom stringr str_remove str_detect
#'
#' @returns Reactive list variable with 4 items
#' - dbPath: path to the database
#' - dbName: (file) name of the database
#' - dbCode: temporary code name of the database
#' - dbType: method chosen from the menu
#'
#' @export
#'
mod_dbSetup_server <- function(
  id,
  localFolder,
  tempFolder,
  schema,
  options = c(1, 2, 3, 4),
  useDB = NULL
) {
  if (1 %in% options) {
    localFolder <- normalizePath(localFolder, mustWork = T)
  }

  if (any(c(2, 3, 4) %in% options)) {
    tempFolder <- normalizePath(tempFolder, mustWork = T)
  }

  schema <- normalizePath(schema, mustWork = T)

  # Modal to show
  dbSelectionModal <- function(localDBs, options) {
    choices = c(
      "Explore a database on the server" = 1,
      "Upload a database from your computer" = 2,
      "Resume with a previously uploaded database" = 3,
      "Start a new database" = 4
    )

    if (missing(options) || is.null(options)) {
      options = 1:length(choices)
    }

    if (!all(options) %in% 1:length(choices)) {
      stop("The DB selection options must be any of", 1:length(choices))
    }

    modalDialog(
      titlePanel("Select a database to get started"),
      radioButtons(
        NS(id, "option"),
        "How would you like to continue ...",
        choices = choices[options],
        width = "100%"
      ),
      if (1 %in% options) {
        conditionalPanel(
          condition = "input.option == '1'",
          selectInput(
            NS(id, "db_local"),
            "Choose an existing database",
            choices = localDBs
          ),
          ns = NS(id)
        )
      },
      if (2 %in% options) {
        conditionalPanel(
          condition = "input.option == '2'",
          fileInput(
            NS(id, "db_upload"),
            "Upload a database from your computer",
            accept = ".db"
          ),
          div(id = sprintf("%s-%s", id, "db_upload_msg")),
          ns = NS(id)
        )
      },
      if (3 %in% options) {
        conditionalPanel(
          condition = "input.option == '3'",
          textInput(NS(id, "db_tempCode"), "Provide temporary database code"),
          ns = NS(id)
        )
      },
      if (4 %in% options) {
        conditionalPanel(
          condition = "input.option == '4'",
          textInput(NS(id, "db_new"), "Database name"),
          ns = NS(id)
        )
      },
      actionButton(NS(id, "start"), "Continue"),
      size = "xl",
      footer = NULL
    )
  }

  # Generate a temp name for a new / uploaded database
  tempDBname <- function() {
    paste0(
      Sys.time() |> as.integer(),
      "_",
      paste(sample(c(LETTERS, letters, 0:9), 4), collapse = "")
    )
  }

  # Connect to a permanent local database
  dbLocal <- function(dbName, localFolder, localDBs, schema) {
    # Check if there are any options available
    if (!dbName %in% localDBs) {
      return(list(
        success = F,
        msg = "The selected database was not found",
        info = NULL
      ))
    }

    # Check if the selected DB is valid
    dbPath <- file.path(localFolder, dbName)
    check <- dbSetup(
      path = dbPath,
      schema = schema,
      validateSchema = T,
      createNew = F,
      showWarning = F
    )

    if (!check$success) {
      return(list(
        success = F,
        msg = "There is an issue with the selected database. Please use another option",
        info = NULL
      ))
    }
    # Update info
    updateQueryString(
      sprintf("?dbmode=local&dbcode=%s", URLencode(dbName, reserved = T)),
      mode = "push"
    )

    return(list(
      success = T,
      msg = "Local database found",
      info = list(
        dbPath = dbPath,
        dbName = dbName,
        dbCode = dbName,
        dbType = 1
      )
    ))
  }

  # --- Upload a database
  dbUpload <- function(uploadInfo, tempFolder, schema) {
    if (is.null(uploadInfo$datapath)) {
      return(list(
        success = F,
        msg = "You must upload a valid database first",
        info = NULL
      ))
    }

    # Check if the selected DB is valid
    check <- dbSetup(
      path = uploadInfo$datapath,
      schema = schema,
      validateSchema = T,
      showWarning = F,
      createNew = F
    )

    if (!check$success) {
      file.remove(uploadInfo$datapath)

      return(list(
        success = F,
        msg = "The provided database does not have the expected schema",
        info = NULL
      ))
    }

    # Move the DB to the designated temp folder with temp name
    nameCode <- tempDBname()
    tempPath <- file.path(
      tempFolder,
      paste0(nameCode, "_", uploadInfo$name, ".db")
    )
    moveDB <- file.copy(uploadInfo$datapath, tempPath)
    file.remove(uploadInfo$datapath)
    dbName <- uploadInfo$name

    # Update info
    updateQueryString(
      sprintf("?dbmode=temp&dbcode=%s", nameCode),
      mode = "push"
    )

    return(list(
      success = T,
      msg = "Database upload successful",
      info = list(
        dbPath = tempPath,
        dbName = dbName,
        dbCode = nameCode,
        dbType = 2
      )
    ))
  }

  # --- Connect to a temporary database with a code
  dbTemp <- function(dbCode, tempFolder, schema) {
    # Check if the database is (still) available
    check <- str_detect(dbCode, "^\\d{10}_\\w{4}$")

    if (!check) {
      return(list(
        success = F,
        msg = "This is not a valid code format",
        info = NULL
      ))
    }

    dbPath <- list.files(tempFolder, pattern = dbCode, full.names = T)
    check <- length(dbPath) > 0 && file.exists(dbPath)

    if (!check) {
      return(list(
        success = F,
        msg = "There is no temporary database with this code",
        info = NULL
      ))
    }

    # Check if the selected DB is still valid
    check <- dbSetup(
      path = dbPath,
      schema = schema,
      validateSchema = T,
      showWarning = F
    )

    if (!check$success) {
      return(list(
        success = F,
        msg = "There is an issue with the selected database. Please use another option",
        info = NULL
      ))
    }

    # Update the modified time stamp to indicate the file is being used again
    Sys.setFileTime(dbPath, Sys.time())

    # Update info
    updateQueryString(
      sprintf("?dbmode=temp&dbcode=%s", dbCode),
      mode = "push"
    )

    return(list(
      success = T,
      msg = "Temp database found",
      info = list(
        dbPath = dbPath,
        dbName = basename(dbPath) |>
          str_remove("^\\d+_[^_]+_") |>
          str_remove("\\.db$"),
        dbCode = dbCode,
        dbType = 3
      )
    ))
  }

  # --- Create a new, temporary database
  dbNew <- function(dbName, tempFolder, schema) {
    # Check the provided name
    check <- dbName |> str_detect("^[\\w-]+$")

    if (!check) {
      return(list(
        success = F,
        msg = paste(
          "The name of the new database can only contain",
          "letters, numbers, underscores or dashes.",
          "No spaces or other special characters.",
          "The file extension .db is added automatically"
        ),
        info = NULL
      ))
    }

    nameCode <- tempDBname()
    tempPath <- file.path(tempFolder, paste0(nameCode, "_", dbName, ".db.db"))
    check <- dbSetup(tempPath, schema = schema)

    updateQueryString(
      sprintf("?dbmode=temp&dbcode=%s", nameCode),
      mode = "push"
    )

    return(list(
      success = T,
      msg = "New database creation successful",
      info = list(
        dbPath = tempPath,
        dbName = paste0.db(dbName, ".db"),
        dbCode = nameCode,
        dbType = 4
      )
    ))
  }

  moduleServer(id, function(input, output, session) {
    # List existing databases
    if (1 %in% options) {
      localDBs <- list.files(localFolder, pattern = ".db")
    } else {
      localDBs <- NULL
    }

    # This reactive will be returned by the module
    connInfo <- reactiveVal()

    # Runs when the module is intialised
    observe({
      if (!is.null(useDB)) {
        # Path has been directly provided
        dir <- normalizePath(dirname(useDB), mustWork = T)
        dbName <- basename(normalizePath(useDB))
        result <- dbLocal(dbName, dir, dbName, schema)
        connInfo(result$info)
      } else if (!"dbmode" %in% names(isolate(getQueryString()))) {
        # No DB data provided, show modal
        showModal(dbSelectionModal(localDBs))
      } else {
        # Check the provided DB info in URL
        dbmode <- getQueryString()$dbmode

        if (dbmode == "local" && (1 %in% options)) {
          dbcode <- URLdecode(getQueryString()$dbcode)
          dbName <- list.files(pattern = dbcode) |> str_remove("\\.db$")
          result <- dbLocal(dbName, localFolder, localDBs, schema)
        } else if (dbmode == "temp" && (any(c(2, 3, 4) %in% options))) {
          result <- dbTemp(getQueryString()$dbcode, tempFolder, schema)
        } else {
          result(list(
            success = F,
            msg = "ERROR - The URL does not point to a valid database"
          ))
        }

        # Set connInfo if valid DB or show modal
        if (result$success) {
          connInfo(result$info)
        } else {
          mod_dbConnect_ui("getDB")
          showNotification(result$msg, type = "error")
        }
      }
    })

    observeEvent(input$start, {
      if (input$option == "1") {
        # --- OPTION 1 - Connect to a permanent local database
        result <- dbLocal(input$db_local, localFolder, localDBs, schema)
        el <- "db_local"
      } else if (input$option == "2") {
        # --- OPTION 2 - Upload a database
        result <- dbUpload(input$db_upload, tempFolder, schema)
        el <- "db_upload_msg"
      } else if (input$option == "3") {
        # --- OPTION 3 - Connect to a temporary database with a code
        result <- dbTemp(input$db_tempCode, tempFolder, schema)
        el <- "db_tempCode"
      } else if (input$option == "4") {
        # --- OPTION 4 - Create a new, temporary database
        result <- dbNew(input$db_new, tempFolder, schema)
        el <- "db_new"
      } else {
        result <- list(info = NULL, success = F, msg = "start")
        el <- "start"
      }

      print(result)

      if (result$success) {
        connInfo(result$info)
        removeModal()
      } else {
        elementMsg(sprintf("%s-%s", id, el), result$msg)
      }
    })

    output$dbDownload <- downloadHandler(
      filename = function() {
        paste0(connInfo()$dbName)
      },
      content = function(file) {
        file.copy(connInfo()$dbPath, file)
      }
    )

    return(connInfo)
  })
}
