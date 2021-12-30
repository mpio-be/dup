

#' uses ID_changes table
#' @param  cnf  configuration variables are obtained from an external file config file. 
#'         default to config::get().
#' @export
BT_at_WESTERHOLZ_change_ID <- function( cnf = config::get() ) {
   
    host = cnf$host$name
    user = cnf$host$dbadmin
    pwd  = cnf$host$dbpwd

    con = dbConnect(RMariaDB::MariaDB(), user = user, password = pwd, host = host, dbname = 'BTatWESTERHOLZ')
    on.exit(dbDisconnect(con))


    d = dbGetQuery(con, 'select * from ID_changes') %>% data.table

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
    
    glue( '{nrow(o)} ID-s updated.')
   

}

#' uses ID_changes table
#' @param  cnf  configuration variables are obtained from an external file config file.
#'         default to config::get().
#' @return Nrows updated in DB_changes
#' @export
RUFF_at_SEEWIESEN_change_ID <- function(cnf = config::get()) {
   host <- cnf$host$name
   user <- cnf$host$dbadmin
   pwd <- cnf$host$dbpwd

   con <- dbConnect(RMariaDB::MariaDB(), user = user, password = pwd, host = host, dbname = "RUFFatSEEWIESEN")
   on.exit(dbDisconnect(con))


   d <- dbGetQuery(con, "select * from ID_changes WHERE datetime_db is NULL") |> data.table()

   if (nrow(d) > 0) {
      upQueries <- d[, .(sql = c(
         glue_data(.SD, "UPDATE ADULTS    SET ID           = '{new_ID}' WHERE ID       = '{old_ID}' "),
         glue_data(.SD, "UPDATE CHICKS    SET ID           = '{new_ID}' WHERE ID       = '{old_ID}' "),
         glue_data(.SD, "UPDATE SEX_and_MORPH SET ID       = '{new_ID}' WHERE ID       = '{old_ID}' "),
         glue_data(.SD, "UPDATE PATERNITY SET ID_father    = '{new_ID}' WHERE ID_father   = '{old_ID}' "),
         glue_data(.SD, "UPDATE PATERNITY SET ID_mother    = '{new_ID}' WHERE ID_mother   = '{old_ID}' "),
         glue_data(.SD, "UPDATE PATERNITY SET ID_offspring = '{new_ID}' WHERE ID_offspring   = '{old_ID}' ")
      )),
      by = "pk"
      ]

      upQueries[, run := dbExecute(con, sql), by = 1:nrow(upQueries)]

      done_updates = upQueries[run == 1]
         
      # update datetime_db in ID_changes
      if (nrow(done_updates) > 0) {
         uq = glue("UPDATE ID_changes set datetime_db = NOW() where pk in ({ paste(done_updates$pk,collapse = ", ") })")
         dbExecute(con, uq)
         o = upQueries[run == 1] |> nrow()
      } else {
          o = 0
          warning("nothing was done although ID_changes indicates otherwise.")
       }
      
   } else {
      o = 0
   }
   
   o

}



 
