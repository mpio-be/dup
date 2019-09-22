# ==========================================================================
# Specialized functions on BTatWESTERHOLZ & FIELD_BTatWESTERHOLZ
# ?? keep here or move to bib ?
# ==========================================================================

#' uses ID_changes table
#' Run this function after each update in ID_changes (run on cron!)
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





 





