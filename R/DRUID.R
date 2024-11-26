to_timestamp <- function(x) {
  format(x, format = "%Y-%m-%dT%H:%M:%SZ")
  }

from_timestamp <- function(x) {
  with_tz(ymd_hms(x), "UTC")
  }



#' Download the latest DRUID data and updates DB
#' Fetch DRUID data
#' @param what GPS, ODBA or ENV
#' @export
#' @examples
#' x = DRUID.downloadNew(what = "GPS")
#' x = DRUID.downloadNew(what = "ENV")

DRUID.downloadNew <- function(what, SERVER = "scidb", interactive = FALSE) {

  crd = config::get(config = "druid_api")
  logString = ecotopia_login(crd$generic$un,  crd$generic$pwd,  crd$kw1, crd$kw2)

  # last time stamps 
  ltt = dbq(q = glue("SELECT id,  max(timestamp) last_timestamp FROM DRUID.{what} GROUP BY id "), server = SERVER)
  ddl = dbq(q = "SELECT * FROM DRUID.device_list", server = SERVER)


  d = merge(ddl, ltt, by = "id", all.x = TRUE)
  d[is.na(last_timestamp), last_timestamp := from_timestamp("2000-01-01T00:00:00Z")]
    

 
  o = foreach(i = 1:nrow(d), .errorhandling = "pass") %do% {
    if (interactive)  print(i)
    
    
    dtm = d[i, last_timestamp]

    oi = ecotopia_data(logString, d[i, id],
      datetime = to_timestamp(dtm - 3600*2), # fetch two hours earlier to prevent data loss
      what = tolower(what),
      verbose = interactive
    )
    
    oi[from_timestamp(timestamp) > dtm]

  }

  O = rbindlist(o[!sapply(o, inherits, what = "error")])
  

  if(nrow(O) > 0) {
  
  if(what == "GPS" && nrow(O) > 0)
    O[latitude == 200, ":="(latitude = NA, longitude = NA, altitude = NA, hdop = NA, vdop = NA)]
 
  setnames(O, "device_id", "id")
  O[, updated_at := from_timestamp(updated_at) ]
  O[,  timestamp :=  from_timestamp(timestamp) ]
  
  con = dbcon(db = "DRUID", server = SERVER)
  ok = DBI::dbWriteTable(con, what, O, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)
  if (ok) n = nrow(O)
  

  } else n = 0

  n

}