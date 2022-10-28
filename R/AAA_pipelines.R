

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




#' @title   DB internal updates pipeline
#' @export
DB_internal_updates.pipeline <- function() {
    
    o = RUFF_at_SEEWIESEN_change_ID()
    push_msg(title = "internal updates", x = glue("{o} ID-s changed in RUFF_at_SEEWIESEN"))
    
    }


#' @title   backup pipeline
#' @export
backup.pipeline <- function(cnf = config::get('host') ) {

    # ini
    con = dbConnect(RMariaDB::MariaDB(), user = cnf$dbadmin, password = cnf$dbpwd, host = cnf$name)
    on.exit(dbDisconnect(con))

    x = dbGetQuery(con, 'select db from DBLOG.backup where state = "freeze"')
    Exclude = c('mysql', 'information_schema', 'performance_schema', x$db)

    a = mysqldump_host(exclude = Exclude)
    push_msg(a, "SCIDB backup")

    b = rm_old_backups(keep = 10)

    push_msg(glue("{length(b)} old backups removed"), "🔴 BACKUP ")

    }

#' RUFFatSEEWIESEN pipelines
#' @export
RUFFatSEEWIESEN_photos.pipeline <- function() {

    con = dbConnect(RSQLite::SQLite(), db)
    on.exit(dbDisconnect(con))

    start_new = ! "ruff_photos" %in% dbListTables(con)
    
    if(start_new) {
        o = RUFF_at_SEEWIESEN_expand_ADULTS()
        o[, img := rw2base64(path), 1:nrow(o)]
        dbWriteTable(con, "ruff_photos", o, row.names = FALSE)
        dbExecute(con, "CREATE INDEX ID on ruff_photos(id)")
        dbExecute(con, "CREATE INDEX sex on ruff_photos(sex)")
        dbExecute(con, "CREATE INDEX morph on ruff_photos(morph)")
        dbExecute(con, "CREATE INDEX date on ruff_photos(date)")

    }
    
    if(!start_new) {
        lastdt = dbGetQuery(con, "select max(date) dt from ruff_photos")$dt |>
                as.Date( origin = "1970-01-01") 
        o = RUFF_at_SEEWIESEN_expand_ADULTS(last_date = lastdt)
        o[, img := rw2base64(path), 1:nrow(o)]
        dbWriteTable(con,"ruff_photos", o, row.names = FALSE, append = TRUE)

    }



}
