

#' uses ID_changes table
#' @param  cnf  configuration variables are obtained from an external file config file. 
#'         default to config::get().
#' @export
RUFF_at_SEEWIESEN_change_ID <- function( cnf = config::get() ) {
   
    host = cnf$host$name
    user = cnf$host$dbadmin
    pwd  = cnf$host$dbpwd

    con = DBI::dbConnect(RMariaDB::MariaDB(), user = user, password = pwd, host = host, dbname = 'RUFFatSEEWIESEN')
    on.exit(dbDisconnect(con))


    d = DBI::dbGetQuery(
       con,
       "SELECT old_ID, new_ID, pk FROM ID_changes WHERE datetime_db IS NULL"
    ) |> setDT()


    d = d[, .(sql = c(
       paste('UPDATE ADULTS    SET ID       =' , shQuote(new_ID) , 'WHERE ID       =' , shQuote(old_ID)) ,
       paste('UPDATE FOUNDERS    SET ID       =' , shQuote(new_ID) , 'WHERE ID       =' , shQuote(old_ID)) ,
       paste('UPDATE CHICKS    SET ID       =' , shQuote(new_ID) , 'WHERE ID       =' , shQuote(old_ID)) ,
       paste('UPDATE SEX_and_MORPH SET ID   =' , shQuote(new_ID) , 'WHERE ID       =' , shQuote(old_ID)) ,
       paste('UPDATE PATERNITY SET ID_father   =' , shQuote(new_ID) , 'WHERE ID_father   =' , shQuote(old_ID)) ,
       paste('UPDATE PATERNITY SET ID_mother   =' , shQuote(new_ID) , 'WHERE ID_mother   =' , shQuote(old_ID)) 
       ) 
       
       )
    , by = 'pk']

    d[, run := DBI::dbExecute(con, sql), by = 1:nrow(d)]

    # when changes were applied then update ID_changes
    pk_timestamp_update = d[run == 1, ]$pk %>%
       unique() %>%
       paste(., collapse = ",")

    if (nchar(pk_timestamp_update) > 1) {
       DBI::dbExecute(
          con,
          paste("UPDATE ID_changes set datetime_db = NOW()
                     WHERE pk in (", pk_timestamp_update, ")")
       )
    }

    o = d[run == 1]
    
    nrow(o)
   

    }





 
