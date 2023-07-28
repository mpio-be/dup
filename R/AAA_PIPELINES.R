#! on error: all pipelines should return FALSE and send a pushover

#' Argos pipeline
#' @export
ARGOS.pipeline <- function() {
    
    task1 = extract_email_attachements(maildir="ARGOS") |> try(silent = TRUE)
    
    task2 = scidbupdate_ARGOS.incoming() |> try(silent = TRUE)

    task3 = scidbupdate_ARGOS.flush_incoming() |> try(silent = TRUE)

    try_outcome(task1, task2, task3, message = "ARGOS.pipeline is failing!")

    }




#' DB internal updates pipeline
#' @export
DB_internal_updates.pipeline <- function() {
    
    task = RUFFatSEEWIESEN.change_ID() |> try(silent = TRUE)
    try_outcome(task, message = "DB_internal_updates.pipeline is failing!")
    
    }


#' backup pipeline
#' @export
backup.pipeline <- function(cnf = config::get('host') ) {

    # ini
    con = dbConnect(RMariaDB::MariaDB(), user = cnf$dbadmin, password = cnf$dbpwd, host = cnf$name)
    on.exit(dbDisconnect(con))

    x = dbGetQuery(con, 'select db from DBLOG.backup where state = "freeze"')
    Exclude = c('mysql', 'information_schema', 'performance_schema', x$db)

    task1 = mysqldump_host(exclude = Exclude) |> try(silent = TRUE)

    task2 = rm_old_backups(keep = 10) |> try(silent = TRUE)

    try_outcome(task1, task2, message = "backup.pipeline is failing!")

    }


#' RUFFatSEEWIESEN pipelines
#' @export
RUFFatSEEWIESEN_photos.pipeline <- function(...) {

    task1 = RUFFatSEEWIESEN.photos_update() |> try(silent = TRUE)

    task2 = RUFFatSEEWIESEN.photos_convert(...) |> try(silent = TRUE)
    
    try_outcome(task1, task2, message = "RUFFatSEEWIESEN_photos.pipeline is failing!")
    





}

#' DRUID pipeline
#' @export
DRUID.pipeline <- function() {

    task1  = DRUID.downloadNew(what = "GPS") |> try(silent = TRUE)
    task2  = DRUID.downloadNew(what = "ODBA") |> try(silent = TRUE)
    task3  = DRUID.downloadNew(what = "ENV") |> try(silent = TRUE)
    
    try_outcome(task1, task2, task3, message = "DRUID.pipeline is failing!")

}
