
#' ARGOS2
#' @param X data.table creaded by ARGOS2.downloadNew
#' @name ARGOS2
NULL
#> NULL


#' @rdname ARGOS2
#' @export
ARGOS2.downloadNew <- function( ) {
  
  crd = config::get(config = "argos_api")
  login = argos_login(un = crd$un, pwd = crd$pwd, wsdl_server = crd$wsdl_server)
  
  ids = argos_devlist(login)
  ids[, platformId := as.integer(platformId)]

  # locationDate, days ago, by platformId
  # will base the subset on bestMsgDate. locationDate can be NA. bestMsgDate is close in time (+-15min) to locationDate.
  ldt = dbq(
    q = "SELECT  platformId, MAX(bestMsgDate) last_bestMsgDate
          FROM ARGOS2.incoming
            GROUP BY platformId",
    db = "ARGOS2",
    server = "scidb"
  )
  ldt[, last_bestMsgDate := force_tz(last_bestMsgDate, tz = "UTC")]
  
  ldt = merge(ids, ldt, by = "platformId", all.x = TRUE)
  ldt[, nbDaysFromNow := difftime(Sys.time(), last_bestMsgDate, units = "days") |> ceiling() |> as.numeric() ]
  ldt[, nbDaysFromNow := nbDaysFromNow + 1]
  ldt[is.na(nbDaysFromNow) , nbDaysFromNow := 20]
  ldt[nbDaysFromNow > 20, nbDaysFromNow := 20]
  setorder(ldt, nbDaysFromNow)

  # Get data
  o = foreach(i = 1:nrow(ldt)) %do% {
    print(i)
    argos_data(login, platformId = ldt[i, platformId], nbDaysFromNow = ldt[i, nbDaysFromNow])
  }
  
  X = rbindlist(o, fill = TRUE, use.names = TRUE)
  
  if (nrow(X) > 0) {

    X = X[, .(
      programNumber, platformId, platformType, platformModel,
      locationDate, bestMsgDate, latitude, longitude, locationClass, errorRadius, semiMajor, semiMinor, orientation,
      satellite, duration, bestLevel,
      gpsSpeed, gpsHeading,
      hdop, compression,
      nopc, nbMessage, frequency,
      altitude
    )]
    X[, bestMsgDate := ymd_hms(bestMsgDate)]

    X = merge(X, ldt, by = "platformId", all.x = TRUE, sort = FALSE)


    X[, keep := TRUE]
    X[bestMsgDate <= last_bestMsgDate, keep := FALSE]

    X = X[(keep)][, keep := NULL]


    X[, locationDate := ymd_hms(locationDate) |> as.character()]
    X[, bestMsgDate  := ymd_hms(bestMsgDate)  |> as.character()]

    X[, last_bestMsgDate := NULL]

  
  } else X = data.table()

  X

}

#' @rdname ARGOS2
#' @export
ARGOS2.update_incoming <- function(x) {

    con = dbcon(db = "ARGOS2", server = "scidb")
    ok = DBI::dbWriteTable(con, "incoming", x, append = TRUE, row.names = FALSE)
    DBI::dbDisconnect(con)


}