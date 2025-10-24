# dbInfo <- "local/test.db"
# schema <- "tests/testthat/testdata/dummy1.sql"
# dbSetup(dbInfo, schema, validateSchema = T)

library(xml2)
library(stringr)
library(purrr)

set_attrs <- function(node, attrs) {
  attrs <- attrs[!sapply(attrs, is.null)]
  for (name in names(attrs)) {
    xml_set_attr(node, name, attrs[[name]])
  }
}

# template <- read_xml("D:/Desktop/singleTable_newRow.drawio")

# Canvas to host the diagram
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
#' @param tableIds (Optional) tables to get info for. If not set all are returned
#'
#' @import stringr dplyr
#' @importFrom purrr map_df
#'
#' @returns Data frame with pos and size info of tables
#'
#' @export
#'
tableGeom <- function(diagram, tableIds) {
  # Get all tableIds if needed
  if (missing(tableIds)) {
    tableIds <- xml_attr(xml_find_all(diagram, ".//mxCell"), "id") |>
      str_match("^table(\\d+)")
    tableIds <- tableIds[, 2] |> as.integer() |> unique()
    tableIds <- tableIds[!is.na(tableIds)]
  }

  # Get the geom info for each table
  map_df(
    tableIds,
    function(tableId) {
      x <- xml_find_first(
        diagram,
        sprintf(".//mxCell[@id='%s']/mxGeometry", paste0("table", tableId))
      ) |>
        xml_attrs()
      lapply(x[names(x) %in% c("x", "y", "width", "height")], as.integer)
    },
    .id = "tableId"
  )
}

#' Get the connection between tables in the diagram
#'
#' @param diagram Diagram to use
#'
#' @import stringr
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

# Table 1
diagram <- diagramCanvas() |>
  diagramTable(
    tableId = 1,
    value = "Table 1"
  ) |>
  diagramRow(1, 1, keyInfo = "PK", name = "id", bottom = 1) |>
  diagramRow(1, 2, name = "name") |>
  diagramRow(1, 3, name = "place") |>
  diagramRow(1, 4, keyInfo = "FK")

# Table 2
diagram <- diagram |>
  diagramTable(
    tableId = 2,
    value = "Table 2",
    x = 280
  ) |>
  diagramRow(2, 1, keyInfo = "PK", name = "id", bottom = 1) |>
  diagramRow(2, 2, name = "name") |>
  diagramRow(2, 3, name = "place") |>
  diagramRow(2, 4, keyInfo = "FK")

# Relationship
diagram <- diagram |> diagramRelationship(2, 4, 1, 1)
diagram <- diagram |> diagramRelationship(1, 4, 2, 1)

as.character(diagram) |> writeLines("D:/Desktop/xml/test.xml")


library(igraph)

diagramLayout <- function(diagram) {
  tableGeom(diagram)
}

g <- graph_from_data_frame(
  data.frame(
    from = c(1, 1, 2, 3, 1),
    to = c(2, 3, 4, 4, 4)
  ),
  directed = F
)
coords <- layout_with_fr(g)
plot(g, layout = coords)
plot(coords)

#Make sure the distance between table origin is large enough
coords <- dist(coords) * sqrt(180^2 + 90^2) / min(dist(coords))
coords <- cmdscale(coord_dist)
# Make pos and screen coords
coords[, 1] <- coords[, 1] - min(min(coords[, 1]), 0)
coords[, 2] <- coords[, 2] - min(min(coords[, 2]), 0)
coords[, 2] <- max(coords[, 2]) - coords[, 2]

plot(coords, asp = 1, ylim = c(max(coords[, 2]), min(coords[, 2])))

# Now we have the coordinates fo the table

# BACKUP
rect <- coords |> cbind(180) |> cbind(60)

xmin <- rect[, 1]
ymin <- rect[, 2] - rect[, 4]
xmax <- rect[, 1] + rect[, 3]
ymax <- rect[, 2]

pos <- cbind(rect[, 1], rect[, 2] - rect[, 4], rect[, 1] + rect[, 3], rect[, 2])

pos[, 1] - pos[, 3]
pos[, 2] - pos[, 4]

rect_distance <- function(r1, r2) {
  x1min <- r1[1]
  y1min <- r1[2]
  x1max <- r1[3]
  y1max <- r1[4]
  x2min <- r2[1]
  y2min <- r2[2]
  x2max <- r2[3]
  y2max <- r2[4]

  # Horizontal and vertical separation (positive if apart)
  dx <- max(x2min - x1max, x1min - x2max)
  dy <- max(y2min - y1max, y1min - y2max)

  if (dx > 0 || dy > 0) {
    # Non-overlapping case → positive distance
    return(sqrt(max(dx, 0)^2 + max(dy, 0)^2))
  } else {
    # Overlapping case → negative overlap depth
    # Overlap distance = smallest amount of "intrusion" along x or y
    overlap_x <- min(x1max, x2max) - max(x1min, x2min)
    overlap_y <- min(y1max, y2max) - max(y1min, y2min)
    return(-min(overlap_x, overlap_y))
  }
}

# Function to compute minimum distance among all rectangles
min_rect_distance <- function(rectangles) {
  n <- length(rectangles)
  min_dist <- Inf

  for (i in seq_len(n - 1)) {
    for (j in seq((i + 1), n)) {
      d <- rect_distance(rectangles[[i]], rectangles[[j]])
      if (d < min_dist) {
        min_dist <- d
      }
    }
  }
  return(min_dist)
}

# Example usage
rectangles <- list(
  c(0, 0, 2, 2),
  c(0, 0, 5, 3),
  c(6, 0, 7, 1)
)


cat("Minimum distance:", min_rect_distance(coords), "\n")
