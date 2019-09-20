# ==========================================================================
# Specialized functions on BTatWESTERHOLZ & FIELD_BTatWESTERHOLZ
# ?? keep here or move to bib ?
# ==========================================================================

#' uses ID_changes table
#' Run this function after each update in ID_changes (run once on cron!)
#' 
#' @export
BT_at_WESTERHOLZ_change_ID <- function( h = getOption('host') ) {
   
    con = dbcon('mihai', host = h , db = 'BTatWESTERHOLZ'); on.exit(dbDisconnect(con))

    d = dbq(con, 'select * from ID_changes')

    d = d[, .(sql = c(
       paste('UPDATE ADULTS    SET ID       =' , shQuote(new_ID) , 'WHERE ID       =' , shQuote(old_ID)) ,
       paste('UPDATE CHICKS    SET ID       =' , shQuote(new_ID) , 'WHERE ID       =' , shQuote(old_ID)) ,
       paste('UPDATE LAB_ID    SET ID       =' , shQuote(new_ID) , 'WHERE ID       =' , shQuote(old_ID)) ,
       paste('UPDATE MICROSATS SET ID       =' , shQuote(new_ID) , 'WHERE ID       =' , shQuote(old_ID)) ,
       paste('UPDATE SEX       SET ID       =' , shQuote(new_ID) , 'WHERE ID       =' , shQuote(old_ID)) ,
       paste('UPDATE PATERNITY SET father   =' , shQuote(new_ID) , 'WHERE father   =' , shQuote(old_ID)) ,
       paste('UPDATE PATERNITY SET mother   =' , shQuote(new_ID) , 'WHERE mother   =' , shQuote(old_ID)) ,
       paste('UPDATE BREEDING  SET IDmale   =' , shQuote(new_ID) , 'WHERE IDmale   =' , shQuote(old_ID)) ,
       paste('UPDATE BREEDING  SET IDfemale =' , shQuote(new_ID) , 'WHERE IDfemale =' , shQuote(old_ID)) ) )
    , by = 'pk']

    d[, run := dbExecute(con, sql), by = 1:nrow(d)]

    # when changes were applied then update ID_changes
    pk_timestamp_update = d[run == 1, ]$pk %>% unique %>% paste(., collapse = ',')

    if(nchar(pk_timestamp_update)> 1)
    dbExecute(con, paste('UPDATE ID_changes set datetime_db = NOW() where pk in (', pk_timestamp_update , ')') )

    o = d[run == 1]
    if(nrow(o) == 0) message('All ID-s are changed, nothing to do.')
    o  

    }



####### OBSOLETE FUNCTIONS BELOW : ?

#' @export
FIELD_BTatWESTERHOLZ_freeze <- function(YEAR = year(Sys.Date()), dryrun = TRUE) {

  newdb = paste0('FIELD_', YEAR , '_BTatWESTERHOLZ')

  fp = mysqldump('FIELD_BTatWESTERHOLZ',  
      user = 'mihai', 
      host = getOption('host') , dir = tempdir(), '--no-create-db', dryrun = FALSE) # dryrun = FALSE because no change to db is done


  mysqlrestore(fp, newdb, 
    user = 'mihai',
    host = getOption('host') , dryrun = dryrun)

  # refresh FIELD_DB
  if(!dryrun) {
    x = dbq(q= "select TABLE_NAME from information_schema.TABLES  WHERE TABLE_SCHEMA ='FIELD_BTatWESTERHOLZ' AND TABLE_TYPE = 'BASE TABLE'",user = 'mihai', host = getOption('host')   )
    x[, DDL := paste0('TRUNCATE TABLE FIELD_BTatWESTERHOLZ.', TABLE_NAME)]
    x[, dbq(q = DDL, user = 'mihai', host = getOption('host') ), by = 1:nrow(x)]
   }


 
 }

 #' @export

#' @export
#' @note IN WORK!!!
FIELD_BTatWESTERHOLZ_CHICKS_TEMPLATE <- function(con ) {

  # con = dbcon('mihai', host = "scidb.mpio.orn.mpg.de", db = "FIELD_2018_BTatWESTERHOLZ")
  
  x = dbq(con, 'select box, date_time, nest_stage, eggs,chicks, guessed, collect_eggs, dead_eggs dead_chicks,nest_failed
                  from NESTS where nest_stage in ("Y", "E") ')



  



 
 }

 





