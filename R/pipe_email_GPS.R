# ==========================================================================
# mti_gps
# ==========================================================================
#' @title   mti_gps.BUTEOatEUROPE
#' @param  cnf  configuration variables are obtained from an external file config file. 
#'         default to config::get().
#' @export
#' @examples
#' scidbupdate_mti_gps.BUTEOatEUROPE()
scidbupdate_mti_gps.BUTEOatEUROPE <- function(cnf = config::get()) {
  
    host = cnf$host$name
    db   = cnf$db$gps
    user = cnf$host$dbadmin
    pwd  = cnf$host$dbpwd

    con = dbConnect(RMariaDB::MariaDB(), user = user, password = pwd, host = host, dbname = db)
    on.exit(dbDisconnect(con))

 
    # already uploaded
    flist = dbGetQuery(con, 'SELECT DISTINCT filenam from mti_gps')$filenam
    # last uploaded date
    lud  = anytime(str_split(flist, "_", simplify = TRUE)[, 2]) %>% max

    # new data
    x = read_email_attachements(maildir='GSM_MTI', pattern = "g_", exclude = flist, lastdate = lud , sepDate = "-")


    if(nrow(x) > 0) { # then prepare data & write to DB
    

        x[Altitude_m == 'Low Voltage', low_voltagge := 1]
        x[Altitude_m == 'Low Voltage', Altitude_m   := NA]
        x[, tagID := str_split(filenam, '_', simplify = TRUE)[,1] %>% str_replace(., 'g', '') %>% as.numeric ]


         x = x[, .(tagID, DateTime, Latitude_N, Longitude_E, Altitude_m, HDOP ,VDOP, SatelliteCount, low_voltagge, filenam)]
        setnames(x, c('tagID','DateTime','latitude','longitude','altitude','HDOP','VDOP','SatelliteCount','low_voltagge', 'filenam') )

        x[, altitude := as.numeric(altitude)]

        return(dbWriteTable(con, 'mti_gps', x, row.names = FALSE, append = TRUE))

        } else FALSE

 }


#' @title mti_sensors.BUTEOatEUROPE
#' @param  cnf  configuration variables are obtained from an external file config file. 
#'         default to config::get().
#' @export
#' @examples
#' scidbupdate_mti_sensors.BUTEOatEUROPE()
scidbupdate_mti_sensors.BUTEOatEUROPE <- function(cnf = config::get()) {
  
    host = cnf$host$name
    db   = cnf$db$gps
    user = cnf$host$dbadmin
    pwd  = cnf$host$dbpwd

    con = dbConnect(RMariaDB::MariaDB(), user = user, password = pwd, host = host, dbname = db)
    on.exit(dbDisconnect(con))

    # already uploaded
    flist = sdb::dbq(con, 'SELECT DISTINCT filenam from mti_sensors')[[1]]
    # last uploaded date
    lud  = anytime(str_split(flist, "_", simplify = TRUE)[, 2]) %>% max

    # new data
    x = read_email_attachements(maildir='GSM_MTI', pattern = "e_", exclude = flist, lastdate = lud,sepDate = "-")


    if(nrow(x) > 0) { # then prepare data & write to DB
    
        x[, tagID := str_split(filenam, '_', simplify = TRUE)[,1] %>% str_replace(., 'e', '') %>% as.numeric ]
        
        setcolorder(x, c(6, 1:5))
        setnames(x, c('tagID' ,'DateTime' ,'temperature', 'BatteryVoltage', 'ActivityCount', 'filenam'))

        return(dbWriteTable(con, 'mti_sensors', x, row.names = FALSE, append = TRUE))

        } else FALSE

 }




#' @title Argos pipeline
#' @export
#' @examples
#' scidbupdate_BUTEOatEUROPE.pipeline()
scidbupdate_BUTEOatEUROPE.pipeline <- function() {

    cat( red$bold('\n ----> Get new emails and extract attachments ......\n') )
   extract_email_attachements(maildir="GSM_MTI")
    
    cat( blue$bold('\n ----> Update gps table.....\n') )
    scidbupdate_mti_gps.BUTEOatEUROPE()

    cat( green$bold('\n ----> Update sensors table....\n') )
    scidbupdate_mti_sensors.BUTEOatEUROPE()


    






    }