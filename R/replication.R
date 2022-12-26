
#' @export
check_replication <- function() {
  
  con = dbcon("primary", db = "DBLOG")

  cons = dbcon("replica", db = "DBLOG")


  DBI::dbExecute(con, "DROP TABLE IF EXISTS DBLOG.replication_test")
  DBI::dbWriteTable(con, "replication_test", data.frame(ts = Sys.time()))
  
   Sys.sleep(1)

  x = dbq(con, "SELECT * FROM DBLOG.replication_test")
  y = try(dbq(cons, "SELECT * FROM DBLOG.replication_test"), silent = TRUE)
  
  closeCon(con)
  closeCon(cons)
  

  # report
  ok = identical(x, y)

  if (!ok) {
    o = difftime(y$ts, x$ts, units = "secs")
    return(o)
    dup::push_msg(glue::glue("Replication is lagging by {o} secs"), "⚠️ WARNING ⚠️")
  }

  

}