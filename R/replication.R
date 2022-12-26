

check_replication <- function(primary, replica) {
  
  con = dbo::dbcon(primary, db = "DBLOG")

  cons = dbo::dbcon(replica, db = "DBLOG")


  dbExecute(con, "DROP TABLE IF EXISTS DBLOG.replication_test")
  dbWriteTable(con, "replication_test", data.frame(ts = Sys.time()) )
  
   Sys.sleep(lag)

  x = dbq(con, "SELECT * FROM DBLOG.replication_test")
  y = try(dbq(cons, "SELECT * FROM DBLOG.replication_test"), silent = TRUE)
  
  closeCon(con)
  closeCon(cons)
  

  # report
  ok = identical(x, y)

  if(ok)
    o = glue("🟢 Replication to {slave} {label} successful at {y$ts}")
    
  if(!ok)
    o = glue("🔴 Replication to {slave} {label} broken. Last attempt {x$ts}") 

  o  


}