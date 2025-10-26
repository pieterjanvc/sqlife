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

# Table 3
diagram <- diagram |>
  diagramTable(
    tableId = 3,
    value = "Table 3"
  ) |>
  diagramRow(3, 1, keyInfo = "PK", name = "id", bottom = 1) |>
  diagramRow(3, 2, keyInfo = "FK")

# Relationship
diagram <- diagram |> diagramRelationship(2, 4, 1, 1)
diagram <- diagram |> diagramRelationship(3, 2, 1, 1)
diagram <- diagram |> diagramRelationship(2, 3, 3, 1)

diagram <- diagramLayout(diagram, padding = 20)

as.character(diagram) |> writeLines("D:/Desktop/xml/test.xml")

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
