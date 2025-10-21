# dbInfo <- "local/test.db"
# schema <- "tests/testthat/testdata/dummy1.sql"
# dbSetup(dbInfo, schema, validateSchema = T)

library(xml2)
library(stringr)

set_attrs <- function(node, attrs) {
  attrs <- attrs[!sapply(attrs, is.null)]
  for (name in names(attrs)) {
    xml_set_attr(node, name, attrs[[name]])
  }
}

# template <- read_xml("D:/Desktop/singleTable_newRow.drawio")

# Start with a root object
diagram <- read_xml("D:/Desktop/xml/skeleton.xml")

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

  # Create new row if not
  if (newRow) {
    template <- read_xml("D:/Desktop/xml/elements.xml")
    mxCell1 <- read_xml(as.character(xml_find_all(template, ".//mxCell")[2]))
    mxCell2 <- read_xml(as.character(xml_find_all(template, ".//mxCell")[3]))
    mxCell3 <- read_xml(as.character(xml_find_all(template, ".//mxCell")[4]))
  }

  tableAttrs <- xml_find_first(
    diagram,
    sprintf(".//mxCell[@id='table%s']/mxGeometry", tableId)
  ) |>
    xml_attrs()

  attrList[["style"]] <- xml_attrs(mxCell1)[["style"]] |>
    str_replace("bottom=\\d", paste0("bottom=", attrList[["bottom"]]))

  # Set the row attributes
  attrList[["parent"]] <- "1"
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

diagram <- diagramTable(
  diagram,
  tableId = 1,
  value = "Table 1",
  x = 160,
  y = 500,
  width = 180, # will clip text if overflow
  height = 120 # 30 + Number of rows * 30
) |>
  diagramRow(1, 1, keyInfo = "PK", name = "id", bottom = 1) |>
  diagramRow(1, 2, name = "name") |>
  diagramRow(1, 3, keyInfo = "FK")


as.character(diagram) |> writeLines("D:/Desktop/xml/test.xml")
