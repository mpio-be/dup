

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

  d[, last_timestamp := format(last_timestamp, format = "%Y-%m-%dT%H:%M:%SZ")]



  o <- foreach(i = 1:nrow(d), .errorhandling = "pass") %do% {
    if (interactive())  print(i)
    x <- ecotopia_data(d[i, lstr], d[i, id], datetime = d[i, last_timestamp], what = tolower(what) , verbose = FALSE)

  }

  sapply(o, inherits, what = "error") |> print()

  O = rbindlist(o[!sapply(o, inherits, what = "error")])
  

  if(nrow(O) > 0) {
  
  if(what == "GPS" && nrow(O) > 0)
    O[latitude == 200, ":="(latitude = NA, longitude = NA, altitude = NA, hdop = NA, vdop = NA)]
 
  setnames(O, "device_id", "id")
  O[, updated_at := with_tz(ymd_hms(updated_at), "UTC")]
  O[, timestamp := with_tz(ymd_hms(timestamp), "UTC")]
  
  con = dbcon(db = "DRUID", server = "scidb")
  ok = DBI::dbWriteTable(con, what, O, append = TRUE, row.names = FALSE)
  DBI::dbDisconnect(con)
  if (ok) n = nrow(O)
  

  } else n = 0

  n

}