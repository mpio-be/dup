
#' sqlitedump 
#' @param  db     db name
#' @param  tables  one of more table names, character vector
#' @param  exclude_columns  which columns to exclude
#' @param  indices  which columns are indices
#' @param  dir    directory path. the destination file (same location on both local and remote).
#' @param  filename   name of the sqlite file.
#' @param  remote when TRUE the file is uploaded to a remote host defined in cnf
#' @param  cnf    configuration variables (host, user, pwd, remotehost) are obtained 
#' 				        from an external file config file. default to config::get().
#'
#' @return path of the dumped file
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' 	require(dup)
#'  Sys.setenv(R_CONFIG_ACTIVE = "localhost")
#' sqlitedump(db = 'ARGOS',
#' tables = c('2019_LBDO', '2020_BADO', '2022_WRSA'), 
#' exclude_columns = c("satellite",  "messageDate", "locationClass", "compressionIndex",  "S1", "S2", "S3", "S4", "S5", #' "S6", "S7", "S8", "filenam"), 
#' indices = c('tagID', 'locationDate')
#' )
#' }
#' 
sqlitedump <- function(db, tables,exclude_columns, indices,
                      dir = '.',
                      filename = glue("{dir}/{db}.sqlite"), 
                      remote = TRUE,
                      cnf = config::get() ) {

    host  = cnf$host$name
    user  = cnf$host$dbadmin
    pwd   = cnf$host$dbpwd
    remoteuser  = cnf$remotehost_1$dbadmin
    remotepwd   = cnf$remotehost_1$syspwd
    remotehost  = cnf$remotehost_1$name

    con = dbConnect(RMariaDB::MariaDB(), user = user, password = pwd, 
          host = host, db = db)
         on.exit(dbDisconnect(con))

    xl = foreach(i=1:length(tables)) %do% {
        ti = dbReadTable(con, tables[i])
        setDT(ti)
      if (!missing(exclude_columns)) {
        ti[, (exclude_columns) := NULL]
      }
      ti

      }

    dbnam = paste(dir, filename, sep = "/")
    if (file.exists(dbnam)) file.remove(dbnam)

    conlite = dbConnect(RSQLite::SQLite(), dbname = paste(dir, filename,sep='/'))

    foreach(i = 1:length(tables) ) %do% {
      dbWriteTable(conlite, tables[i], xl[[i]], row.names = FALSE)
      
      if(!missing(indices)) {
        for( j in 1:length(indices) )
          dbExecute(conlite, 
            glue("CREATE INDEX {indices[j]}_{tables[i]}_index ON `{tables[i]}` ( {indices[j]});")
          )
      }

    }

    dbExecute(conlite, "CREATE TABLE dump_date (dumpdate DATETIME)")
    dbExecute(conlite, "INSERT INTO dump_date (dumpdate) VALUES(DateTime('now'))")

    if(remote) {
      ss = ssh_connect( glue('{remoteuser}@{remotehost}'), passwd =  remotepwd)
      scp_upload(ss, filename, to = dir, verbose = TRUE)
      ssh_disconnect(ss)
      }

  filename

   }
