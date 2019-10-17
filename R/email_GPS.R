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
    
    Start=Sys.time()    

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

       n_rows = dbWriteTable(con, 'mti_gps', x, row.names = FALSE, append = TRUE)

        } else n_rows = 0


    tt = difftime(Sys.time(), Start, units = 'mins') %>% round %>% as.character
        
    msg = paste(
    glue('🕘  {tt} mins'), 
    glue('🔄  mti_gps got {n_rows} rows '), 
    sep = '\n'
    )

    msg




 }


#' @title mti_sensors.BUTEOatEUROPE
#' @param  cnf  configuration variables are obtained from an external file config file. 
#'         default to config::get().
#' @export
#' @examples
#' scidbupdate_mti_sensors.BUTEOatEUROPE()
scidbupdate_mti_sensors.BUTEOatEUROPE <- function(cnf = config::get()) {
    
    Start=Sys.time() 

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

        n_rows = dbWriteTable(con, 'mti_sensors', x, row.names = FALSE, append = TRUE)

        } else n_rows = 0

    tt = difftime(Sys.time(), Start, units = 'mins') %>% round %>% as.character
        
    msg = paste(
    glue('🕘  {tt} mins'), 
    glue('🔄  mti_sensors got {n_rows} rows '), 
    sep = '\n'
    )

    msg


 }




