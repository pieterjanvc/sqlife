source("renv/activate.R")
if (Sys.getenv("POSITRON") == "1" && .Platform$OS.type == "unix") {
  addTaskCallback(function(...) {
    options(browser = "xdg-open")
    FALSE
  })
}
