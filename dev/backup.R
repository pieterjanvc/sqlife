# Experimenting to close on exit but only on error
dbGC <- function(dbInfo, frame = parent.frame()) {
  conn <- dbGetConn(dbInfo)
  print(frame)
  eval(bquote(on.exit(closeIt(frame))), envir = frame)
  conn
}

test <- function() {
  x <- F
  conn <- dbGC(dbInfo)
  # dbFinish(conn, error = "test")
  # dbFinish(conn)
  Sys.sleep(2)
  print(x)
}

test <- function(frame) {
  objs <- mget(ls(envir = frame), envir = frame)
  cons <- Filter(function(x) inherits(x, "SQLiteConnection"), objs)
  sapply(cons, function(conn) {
    dbFinish(conn, commit = F, closeExisting = T)
  })
}
