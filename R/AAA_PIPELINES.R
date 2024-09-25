#' Database Unattended Pipelines 
#' @note On error all pipelines should return FALSE and send a pushover
#' @name dup
NULL
#> NULL


#' Argos pipeline
#' @export
#' @name dup
ARGOS.pipeline <- function() {
    
    task1 = extract_email_attachements(maildir="ARGOS") |> try(silent = TRUE)
    
    task2 = scidbupdate_ARGOS.incoming() |> try(silent = TRUE)

    task3 = scidbupdate_ARGOS.flush_incoming() |> try(silent = TRUE)

    try_outcome(task1, task2, task3, message = "ARGOS.pipeline is failing!")

    }




#' DB internal updates pipeline
#' @export
#' @name dup
DB_internal_updates.pipeline <- function() {
    
    task = RUFFatSEEWIESEN.change_ID() |> try(silent = TRUE)
    try_outcome(task, message = "DB_internal_updates.pipeline is failing!")
    
    }


#' backup pipeline
#' @export
#' @name dup
backup.pipeline <- function(cnf = config::get('host') ) {

    con <- dbcon(server = "scidb")
    stopifnot(con@host == "scidb.mpio.orn.mpg.de")
    on.exit(dbDisconnect(con))

    x <- dbq(con, 'SELECT db from DBLOG.backup WHERE state = "freeze"')
    Exclude <- c("mysql", "information_schema", "performance_schema", x$db)

    task1 = mysqldump_host(exclude = Exclude) |> try(silent = TRUE)

    task2 = rm_old_backups(keep = 10) |> try(silent = TRUE)

    try_outcome(task1, task2, message = "backup.pipeline is failing!")

    }


#' RUFFatSEEWIESEN pipelines
#' @export
#' @name dup
RUFFatSEEWIESEN_photos.pipeline <- function(...) {

    task1 = RUFFatSEEWIESEN.photos_update() |> try(silent = TRUE)

    task2 = RUFFatSEEWIESEN.photos_convert(...) |> try(silent = TRUE)
    
    try_outcome(task1, task2, message = "RUFFatSEEWIESEN_photos.pipeline is failing!")

}

#' DRUID pipeline
#' @export
#' @name dup
Argos2.pipeline <- function() {

    task1  = ARGOS2.downloadNew() |> try(silent = TRUE)
    task2  = ARGOS2.update_incoming(task1) |> try(silent = TRUE)
 
    try_outcome(task1, task2, message = "ARGOS2.pipeline is failing!")
}
