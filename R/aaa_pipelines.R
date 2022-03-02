

#' @title Argos pipeline
#' @export
ARGOS.pipeline <- function() {
    
    cat( red$bold('\n ----> Get new emails and extract attachments ......\n') )
    extract_email_attachements(maildir="ARGOS")
    
    cat( blue$bold('\n ----> Read email attachments and update incoming table.....\n') )
    a = scidbupdate_ARGOS.incoming()

    cat( green$bold('\n ----> Distribute data from incoming table to YYYY_SPECIES table.\n') )
    b = scidbupdate_ARGOS.flush_incoming()

    # feedback
    m = glue(a, b)
    push_msg(m, 'ARGOS')

    }


#' @title   SNB pipeline
#' @export
SNBatWESTERHOLZ2_pipeline <- function() {


    a = scidb_snbUpdater.b000()
    Sys.sleep(5)
    b = scidb_snbUpdater.transponders()

    # feedback
    m = glue(a,b)
    push_msg(m, 'SNBatWESTERHOLZ2_pipeline')
  
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

    # ini
    con = dbConnect(RMariaDB::MariaDB(), user = cnf$dbadmin, password = cnf$dbpwd, host = cnf$name)
    on.exit(dbDisconnect(con))

    x = dbGetQuery(con, 'select db from DBLOG.backup where state = "freeze"')
    exclude = c('mysql', 'information_schema', 'performance_schema', x$db)

    a = mysqldump_host(exclude = exclude, parallel = TRUE)
    b = rm_old_backups(keep = 10)

    # feedback
    m = glue::glue("{a} scidb backup; {b} old backups trashed.")
    push_msg(m, "SCIDB backup")


    }


#' @title   Export Argos for mapping
#' @export
export_to_mapping.pipeline <- function(db_tabs = c("2019_LBDO", "2020_BADO", "2022_WRSA"),... ) {

    sqlitedump(
        db = "ARGOS",
        tables = db_tabs,
        exclude_columns = c(
            "satellite", "messageDate", "locationClass",
            "compressionIndex", "filenam",
            "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"
        ),
        indices = c("tagID", "locationDate"), 
        fun = dup::speed_along,
        ...
    )

    # feedback
    push_msg(m, "SCIDB backup")


    }
