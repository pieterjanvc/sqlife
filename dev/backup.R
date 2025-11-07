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


fun1 <- function(x = parent.frame()) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), "local/test.db")
  attr(conn, "test") <- F
  withr::defer_parent(
    {
      print(attr(get("conn", envir = x), "test"))
      DBI::dbDisconnect(get("conn", envir = parent.frame()))
    },
    priority = "last"
  )
  conn
}


fun3 <- function(x) {
  nm <- deparse(substitute(x))
  attr(x, "test") <- T
  assign(nm, x, envir = parent.frame())
}


fun2 <- function() {
  conn <- fun1()
  print(attr(conn, "test"))
  fun3(conn)
  print(attr(conn, "test"))
}

fun2()

fun2 <- function(conn) {
  if (missing(conn)) {
    conn <- dbGetConn("local/test.db")
  }
  conn <- dbGetConn(conn)

  . <- tbl(conn, "users") |> collect()
  # stop("err")
  dbFinish(conn)
  print(attributes(conn))
  # browser()
  print("HI")
}

conn <- dbGetConn("local/test.db")

conn <- dbGetConn(conn)
fun2()

dbFinish(conn)

f <- function() {
  setNames(F, sub("^<environment: (.*)>$", "\\1", format(environment())))
}
f()


fun3 <- function(dbInfo) {
  conn <- dbGetConn(dbInfo)
  test <- sub("^<environment: (.*)>$", "\\1", format(environment()))
  browser()
  tbl(conn, "users") |> collect() |> print()
  dbFinish(conn)
}

fun3("local/test.db")


# dbGetConn will add the env ID as conn attr and set it to FALSE (i.e. not finished)
# dbFinish will check if the env is listed in attr
# - if yes, then it can set it to T indicating it finished
# - if no, the connection was passed without dbGetConn at start of current env
# On exit() using defer_parent will check if attr is T or F for current
# - if F  then dbFinish was not used and error should be thrown
# - it T then OK and can remove env from attr
# - if missing then conn was passed with no dbGetConn in curr env (throw error too?)

fun1 <- function(
  env = parent.frame(),
  parFun = as.character(sys.call(sys.parent()))[1]
) {
  print(parFun)
  print(as.character(sys.call(sys.parent()))[1])
}

fun1()

fun2 <- function() {
  fun1()
}

fun2()
