

#' @title Argos pipeline
#' @export
ARGOS.pipeline <- function() {
    Start = Sys.time()
    
    cat( red$bold('\n ----> Get new emails and extract attachments ......\n') )
    extract_email_attachements(maildir="ARGOS")
    
    cat( blue$bold('\n ----> Read email attachments and update incoming table.....\n') )
    scidbupdate_ARGOS.incoming()

    cat( green$bold('\n ----> Distribute data from incoming table to YYYY_SPECIES table.\n') )
    o = scidbupdate_ARGOS.flush_incoming()

    msg = glue("{mins_taken(Start)}\n 
                flush_incoming returns: {o}")


    push_msg(msg, 'ARGOS.pipeline')


    }

#' @title GPS data pipeline
#' @export
BUTEOatEUROPE.pipeline <- function() {
    Start = Sys.time()
    cat( red$bold('\n ----> Get new emails and extract attachments ......\n') )
    extract_email_attachements(maildir="GSM_MTI")

    cat( blue$bold('\n ----> Update gps table.....\n') )
    a = scidbupdate_mti_gps.BUTEOatEUROPE()

    cat( green$bold('\n ----> Update sensors table....\n') )
    b = scidbupdate_mti_sensors.BUTEOatEUROPE()


    msg = glue("{mins_taken(Start)}\n  scidbupdate_mti_gps.BUTEOatEUROPE returns: {a}\n scidbupdate_mti_sensors.BUTEOatEUROPE: {b}\n")


    push_msg(msg, 'BUTEOatEUROPE.pipeline')



    }

#' @title   SNB pipeline
#' @export
SNBatWESTERHOLZ2_pipeline <- function() {


    a = scidb_snbUpdater.b000()
    
    Sys.sleep(5)

    b = scidb_snbUpdater.transponders()


    msg = glue("scidb_snbUpdater.b000 returns: {a}\nscidb_snbUpdater.transponders: {paste(b, collapse = ';')}")


    push_msg(msg, 'SNBatWESTERHOLZ2_pipeline')



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

    # remove old backups
    b = rm_old_backups(keep = 5)


    msg = glue("mysqldump_host returns:\n {paste(paste(names(a), a, sep = '='), collapse = ' \n ')}\nrm_old_backups returns: {b}")


    push_msg(msg, 'backup.pipeline')


    }


