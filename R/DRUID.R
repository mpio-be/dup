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

DRUID.downloadNew <- function(what) {

  crd = config::get(config = "druid_api")
  logK <- ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1, crd$kw2)
  logA <- ecotopia_login(crd$pesa2022$un, crd$pesa2022$pwd, crd$kw1, crd$kw2)
  logL <- ecotopia_login(crd$dsp2022$un, crd$dsp2022$pwd, crd$kw1, crd$kw2)
 
  d = dbq(q = glue("SELECT x.id, d.program_id,x.last_timestamp FROM
                    (SELECT id,  max(timestamp) last_timestamp FROM DRUID.{what} GROUP BY id ) x
                      JOIN DRUID.device_list d on x.id = d.id"), server = "scidb")
    
  d[program_id == "Kempenaers", lstr := logK]
  d[program_id == "aaulsebrook", lstr := logA]
  d[program_id == "luke.eberhart", lstr := logL]

 
  o <- foreach(i = 1:nrow(d), .errorhandling = "pass") %do% {
    if (interactive())  print(i)
    
    
    dtm = d[i, last_timestamp]

    oi = ecotopia_data(d[i, lstr], d[i, id],
      datetime = to_timestamp(dtm - 3600), # fetch one hour earlier to prevent data loss
      what = tolower(what),
      verbose = FALSE
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
  
  con = dbcon(db = "DRUID", server = "scidb")
  ok = DBI::dbWriteTable(con, what, O, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)
  if (ok) n = nrow(O)
  

  } else n = 0

  n

}