

#' @title Argos pipeline
#' @export
ARGOS.pipeline <- function() {
    
    cat( red$bold('\n ----> Get new emails and extract attachments ......\n') )
    extract_email_attachements(maildir="ARGOS")
    
    cat( blue$bold('\n ----> Read email attachments and update incoming table.....\n') )
    a = scidbupdate_ARGOS.incoming()

    cat( green$bold('\n ----> Distribute data from incoming table to YYYY_SPECIES table.\n') )
    b = scidbupdate_ARGOS.flush_incoming()


    push_msg(a, 'ARGOS.incoming')
    push_msg(b, 'ARGOS.flush_incoming')


    }

#' @title GPS data pipeline
#' @export
BUTEOatEUROPE.pipeline <- function() {

    cat( red$bold('\n ----> Get new emails and extract attachments ......\n') )
    extract_email_attachements(maildir="GSM_MTI")

    cat( blue$bold('\n ----> Update gps table.....\n') )
    a = scidbupdate_mti_gps.BUTEOatEUROPE()
    push_msg(a, 'BUTEOatEUROPE.mti_gps')


    cat( green$bold('\n ----> Update sensors table....\n') )
    b = scidbupdate_mti_sensors.BUTEOatEUROPE()
    push_msg(b, 'BUTEOatEUROPE.mti_sensors')


    }

#' @title   SNB pipeline
#' @export
SNBatWESTERHOLZ2_pipeline <- function() {


    a = scidb_snbUpdater.b000()
    push_msg(a, 'SNBatWESTERHOLZ2_pipeline: b000')
    
    Sys.sleep(5)

    b = scidb_snbUpdater.transponders()


    push_msg(b, 'SNBatWESTERHOLZ2_pipeline: transponders')



    }


#' @title   DB internal updates pipeline
#' @export
DB_internal_updates.pipeline <- function() {
    
    o = BT_at_WESTERHOLZ_change_ID()
    push_msg(o, 'DB_internal_updates.pipeline')
    
    }


#' @title   backup pipeline
#' @export
backup.pipeline <- function(cnf = config::get('host') ) {
    Start = Sys.time()
    # ini
    con = dbConnect(RMariaDB::MariaDB(), user = cnf$dbadmin, password = cnf$dbpwd, host = cnf$name)
    on.exit(dbDisconnect(con))

    x = dbGetQuery(con, 'select db from DBLOG.backup where state = "freeze"')
    exclude = c('mysql', 'information_schema', 'performance_schema', x$db)


    # backup
    a = mysqldump_host(exclude = exclude, parallel = TRUE )
    push_msg(glue('🌠 {a} scidb backup'), 'DB backup' )


    # remove old backups
    b = rm_old_backups(keep = 10)
    push_msg(glue('🐪 {b} old backups trashed.'), 'rm old backups' )


    


    }


#' @title   all pipelines
#' @export
pipelines <- function() {
    started.at=Sys.time()    

    ARGOS.pipeline()
    BUTEOatEUROPE.pipeline()
    SNBatWESTERHOLZ2_pipeline()
    DB_internal_updates.pipeline()
    backup.pipeline()

    tt = difftime(Sys.time(), started.at, units = 'hour') %>% round(digits = 2) %>% as.character

    msg = glue('🕘  {tt}  hours')

    push_msg(glue('☃ {msg}  to complete all pipelines!'), 'ALL PIPELINES')


    }

