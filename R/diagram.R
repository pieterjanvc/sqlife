#' Set multiple node attributes in an XML diagram
#'
#' @param node Node object to update
#' @param attrs named list of attributes and values
#'
#' @importFrom xml2 xml_set_attr
#'
#' @returns Nothing
#' @export
#'
set_attrs <- function(node, attrs) {
  attrs <- attrs[!sapply(attrs, is.null)]
  for (name in names(attrs)) {
    xml_set_attr(node, name, attrs[[name]])
  }
}

#' Canvas to host the diagram
#'
#' @param diagram (Optional) Diagram object. If missing new one is created
#' @param ... Named list of parameters to update: id, name, pageWidth, pageHeight
#'
#' @importFrom xml2 xml_find_first
#'
#' @returns Updated / new diagram
#' @export
#'
diagramCanvas <- function(diagram, ...) {
  attrList <- list(...)

  if (missing(diagram)) {
    diagram <- read_xml("D:/Desktop/xml/skeleton.xml")
  }

  diagramNode <- xml_find_first(diagram, ".//diagram")
  set_attrs(diagramNode, attrList[c("id", "name")])

  mxGraphModel <- xml_find_first(diagramNode, ".//mxGraphModel")
  set_attrs(diagramNode, attrList[c("pageWidth", "pageHeight")])

  return(diagram)
}

#' Get the position and size diagram tables
#'
#' @param diagram Diagram to use
#' @param tableIds (Optional) Table ids to get info for. If not set all are returned
#' @param values (Optional) Data frame with columns tableId and any of
#' x, y, width or height to update
#'
#' @import stringr dplyr
#' @importFrom purrr map_df
#' @importFrom xml2 xml_find_first xml_find_all xml_attrs
#'
#' @returns Data frame with pos and size info of tables
#'
#' @export
#'
tableGeom <- function(diagram, tableIds, values) {
  updateVals <- !missing(values)
  # In case values are supplies to set
  if (updateVals) {
    if (!"tableId" %in% colnames(values)) {
      stop("The values datafame must have a column tableId")
    }

    check <- setdiff(
      colnames(values),
      c("tableId", "x", "y", "width", "height")
    )

    if (length(check) > 0) {
      warning(
        "The following value attributes were not found and are ignored: ",
        paste(check, collapse = ", ")
      )
    }

    values <- values |>
      select(any_of(c("tableId", "x", "y", "width", "height")))
  }

  # Get all tableIds if needed
  if (missing(tableIds) & !updateVals) {
    tableIds <- xml_attr(xml_find_all(diagram, ".//mxCell"), "id") |>
      str_match("^table(\\d+)")
    tableIds <- tableIds[, 2] |> as.integer() |> unique()
    tableIds <- tableIds[!is.na(tableIds)]
  } else if (updateVals) {
    tableIds <- values$tableId
  }

  map_df(
    tableIds,
    function(tableId) {
      # Get the table node
      x <- xml_find_first(
        diagram,
        sprintf(".//mxCell[@id='%s']/mxGeometry", paste0("table", tableId))
      )

      # Update if needed
      if (updateVals) {
        set_attrs(
          x,
          as.list(
            values |> filter(tableId == {{ tableId }}) |> select(-tableId)
          )
        )
      }

      # Get the geom values
      x <- xml_attrs(x)
      lapply(x[names(x) %in% c("x", "y", "width", "height")], as.integer)
    }
  ) |>
    mutate(tableId = tableIds, .before = 1)
}

#' Get the connection between tables in the diagram
#'
#' @param diagram Diagram to use
#'
#' @import stringr
#' @importFrom xml2 xml_find_all xml_attr
#'
#' @returns Data frame with links between table rows
#'
#' @export
#'
tableConnections <- function(diagram) {
  #Extract the edges based on how IDs are built
  conns <- xml_attr(xml_find_all(diagram, ".//mxCell"), "id") |>
    str_match("^table(\\d+)_(\\d+)-table(\\d+)_(\\d+)")
  # Create data frame with info
  conns <- conns[!is.na(conns[, 1]), 2:5]
  storage.mode(conns) <- "integer"
  conns <- conns |> as.data.frame()
  colnames(conns) <- c("FKtable", "FKrow", "PKtable", "PKrow")

  return(conns)
}

#' Create / update a table in the diagram
#'
#' @param diagram Diagram object
#' @param tableId Existing or new table Id (integer value)
#' @param ... Named list of parameters to update: value (name displayed),
#' x, y, width and height
#'
#' @importFrom xml2 read_xml xml_find_first xml_find_all xml_add_child
#'
#' @returns Updated diagram
#' @export
#'
diagramTable <- function(diagram, tableId, ...) {
  attrList <- list(...)

  check <- setdiff(
    names(attrList),
    c("value", "x", "y", "width", "height")
  )

  attrList[["id"]] <- sprintf("table%i", tableId)

  if (length(check) > 0) {
    warning(
      "The following attributes were not found and ignored: ",
      paste(check, collapse = ", ")
    )
  }

  # Check if table exists
  mxCell <- xml_find_first(
    diagram,
    sprintf(".//mxCell[@id='%s']", attrList[["id"]])
  )
  newTable <- is.na(mxCell)

  # Create new on if not
  if (newTable) {
    template <- read_xml("D:/Desktop/xml/elements.xml")
    mxCell <- read_xml(as.character(xml_find_all(template, ".//mxCell")[1]))
  }

  # Set the attributes
  set_attrs(mxCell, attrList[c("id", "value")])
  mxGeometry <- xml_find_first(mxCell, ".//mxGeometry")
  set_attrs(mxGeometry, attrList[c("x", "y", "width", "height")])

  # Add table if new
  if (newTable) {
    root <- xml_find_first(diagram, "//root")
    xml_add_child(root, mxCell)
  }

  return(diagram)
}

#' Create / update a table in the diagram
#'
#' @param diagram Diagram object
#' @param tableId Existing table Id (integer value)
#' @param rowNumber Row number in the table (integer value)
#' @param ... Optional named list of parameters to update:
#' - keyInfo: text value of left column
#' - name: row text value
#' - bottom: 0 or 1 to draw a line below row
#'
#' @import stringr
#' @importFrom xml2 xml_find_first xml_find_all read_xml xml_attrs xml_add_child
#'
#' @returns Updated diagram
#' @export
#'
diagramRow <- function(diagram, tableId, rowNumber, ...) {
  attrList <- list(...)
  check <- setdiff(
    names(attrList),
    c("keyInfo", "name", "bottom")
  )

  if (length(check) > 0) {
    warning(
      "The following attributes were not found and ignored: ",
      paste(check, collapse = ", ")
    )
  }

  rowId <- sprintf("table%i_%i", tableId, rowNumber)
  attrList[["id"]] <- rowId

  # Check if row exists
  mxCell1 <- xml_find_first(
    diagram,
    sprintf(".//mxCell[@id='%s']", attrList[["id"]])
  )
  mxCell2 <- xml_find_first(
    diagram,
    sprintf(".//mxCell[@id='%s.1']", attrList[["id"]])
  )
  mxCell3 <- xml_find_first(
    diagram,
    sprintf(".//mxCell[@id='%s.2']", attrList[["id"]])
  )
  newRow <- is.na(mxCell1)

  # Table nodes / attr
  tableNode <- xml_find_first(
    diagram,
    sprintf(".//mxCell[@id='table%s']", tableId)
  )
  tableGeom <- xml_find_first(tableNode, ".//mxGeometry")
  tableAttrs <- tableNode |> xml_attrs() |> append(tableGeom |> xml_attrs())

  # Create new row if not
  if (newRow) {
    template <- read_xml("D:/Desktop/xml/elements.xml")
    mxCell1 <- read_xml(as.character(xml_find_all(template, ".//mxCell")[2]))
    mxCell2 <- read_xml(as.character(xml_find_all(template, ".//mxCell")[3]))
    mxCell3 <- read_xml(as.character(xml_find_all(template, ".//mxCell")[4]))

    # Make the table longer for the new row
    tableAttrs[["height"]] <- as.integer(tableAttrs[["height"]]) + 30
    set_attrs(tableGeom, tableAttrs["height"])
  }

  attrList[["style"]] <- xml_attrs(mxCell1)[["style"]] |>
    str_replace("bottom=\\d", paste0("bottom=", attrList[["bottom"]]))

  # Set the row attributes
  attrList[["parent"]] <- tableAttrs[["id"]]
  set_attrs(mxCell1, attrList[c("id", "parent", "style")])

  #Place the row in the table at the correct position
  mxGeometry <- xml_find_first(mxCell1, ".//mxGeometry")
  attrList[["x"]] <- tableAttrs[["x"]]
  attrList[["y"]] <- as.integer(tableAttrs[["y"]]) + 30 * rowNumber
  attrList[["width"]] <- tableAttrs[["width"]]
  set_attrs(mxGeometry, attrList[c("x", "y", "width")])

  attrList[["id"]] <- paste0(rowId, ".1")
  attrList[["parent"]] <- rowId
  attrList[["value"]] <- attrList[["keyInfo"]]
  set_attrs(mxCell2, attrList[c("id", "parent", "value")])

  attrList[["id"]] <- paste0(rowId, ".2")
  attrList[["value"]] <- attrList[["name"]]
  set_attrs(mxCell3, attrList[c("id", "parent", "value")])

  # Add row if new
  if (newRow) {
    root <- xml_find_first(diagram, "//root")
    for (mxCell in list(mxCell1, mxCell2, mxCell3)) {
      xml_add_child(root, mxCell)
    }
  }

  return(diagram)
}

#' Add / update a relationship between tables
#'
#' @param diagram Diagram to update
#' @param FKtable Integer ID of the foreign key table
#' @param FKrow Integer ID of the row of the foreign key
#' @param PKtable Integer ID of the primary key table
#' @param PKrow Integer ID of the row of the primary key
#' @param ... (Optional) Named list of parameters to update
#'
#' @import stringr
#' @importFrom xml2 xml_find_first xml_find_all read_xml xml_add_child xml_attr
#'
#' @returns Updated diagram
#' @export
#'
diagramRelationship <- function(diagram, FKtable, FKrow, PKtable, PKrow, ...) {
  attrList <- list(...)

  # Check if edge exists
  edgeID <- sprintf("table%i_%i-table%i_%i", FKtable, FKrow, PKtable, PKrow)
  mxCell <- xml_find_first(diagram, sprintf(".//mxCell[@id='%s']", edgeID))
  newEdge <- is.na(mxCell)

  # Create new on if not
  if (newEdge) {
    template <- read_xml("D:/Desktop/xml/elements.xml")
    mxCell <- read_xml(as.character(xml_find_all(template, ".//mxCell")[5]))
  }

  attrList[["style"]] <- xml_attr(mxCell, "style")

  # Connection in-out is based on table x-axis orientation
  if (tableGeom(diagram, FKtable)$x > tableGeom(diagram, PKtable)$x) {
    source <- sprintf("table%i_%i", PKtable, PKrow)
    target <- sprintf("table%i_%i", FKtable, FKrow)
  } else {
    # Flip the one to many
    source <- sprintf("table%i_%i", FKtable, FKrow)
    target <- sprintf("table%i_%i", PKtable, PKrow)

    attrList[["style"]] <- str_replace(
      attrList[["style"]],
      "endArrow=ERoneToMany",
      "endArrow=none"
    )

    attrList[["style"]] <- str_replace(
      attrList[["style"]],
      "startArrow=none",
      "startArrow=ERoneToMany"
    )
  }

  # Set the attributes
  attrList[["id"]] <- edgeID
  attrList[["source"]] <- source
  attrList[["target"]] <- target
  set_attrs(mxCell, attrList[c("id", "source", "target", "style")])

  # Add table if new
  if (newEdge) {
    root <- xml_find_first(diagram, "//root")
    xml_add_child(root, mxCell)
  }

  return(diagram)
}

#' Optimise the diagram layout to minimise table / connections overlap
#'
#' @param diagram Diagram update the layout (tables / relationships)
#' @param padding (Default = 20) Extra padding between tables
#'
#' @importFrom igraph graph_from_data_frame layout_with_fr V
#'
#' @returns Updated diagram
#' @export
#'
diagramLayout <- function(diagram, padding = 20) {
  pos <- tableGeom(diagram)

  # Get the relationships / connections betweem tables, ignore multiple
  rel <- tableConnections(diagram) |>
    mutate(
      from = ifelse(FKtable < PKtable, FKtable, PKtable),
      to = ifelse(FKtable > PKtable, FKtable, PKtable),
      .keep = "none"
    ) |>
    distinct()

  # Create a graph
  g <- graph_from_data_frame(rel, directed = F)
  # Layout to avoid as much edge overlap
  coords <- layout_with_fr(g)
  #Make sure the distance between table origin is large enough
  coords <- dist(coords) *
    sqrt(max(pos$width)^2 + max(pos$height)^2) /
    min(dist(coords)) +
    padding

  if (length(coords) == 1) {
    coords = data.frame(x = c(0, coords[1]), y = c(0, 0))
  } else {
    coords <- cmdscale(coords, 2)
    # Make pos and screen coords
    coords[, 1] <- coords[, 1] - min(min(coords[, 1]), 0)
    coords[, 2] <- coords[, 2] - min(min(coords[, 2]), 0)
    coords[, 2] <- max(coords[, 2]) - coords[, 2]
  }

  # plot(coords, asp = 1, ylim = c(max(coords[, 2]), min(coords[, 2])))

  #Update the layout
  . <- tableGeom(
    diagram,
    values = data.frame(
      tableId = V(g) |> names() |> as.integer(),
      x = coords[, 1] |> round(),
      y = coords[, 2] |> round()
    )
  )

  return(diagram)
}
