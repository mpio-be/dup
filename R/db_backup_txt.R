
#' txtdump
#' @param  db     db name
#' @param  table  table name
#' @param  dir    directory path
#' @param  remote when TRUE the file is uploaded to a remote host defined in cnf
#' @param  cnf  configuration variables (host, user, pwd, remotehost) are obtained 
#' 				from an external file config file. default to config::get().
#'
#' @return TRUE on success FALSE on failure
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' 	require(dup)
#'  Sys.setenv(R_CONFIG_ACTIVE = "localhost")
#'  txtdump(db = 'ARGOS', table = '2019_LBDO')
#' }
#' 
txtdump <- function(db, table, remote = TRUE, dir = '~', cnf = config::get() ) {

		host  = cnf$host$name
		user  = cnf$host$dbadmin
		pwd   = cnf$host$dbpwd
		remoteuser  = cnf$remotehost_1$dbadmin
		remotepwd   = cnf$remotehost_1$syspwd
		remotehost  = cnf$remotehost_1$name

		con = dbConnect(RMariaDB::MariaDB(), user = user, password = pwd, 
					host = host, db = db)
   	    on.exit(dbDisconnect(con))

		x = dbReadTable(con, table)
		setDT(x)

		if(!remote) {
			path = glue('{dir}/{table}.csv')
			fwrite(x, path , yaml = TRUE )
		}

		if(remote) {
			path = glue('{tempdir()}/{table}.csv')
			fwrite(x, path , yaml = TRUE )
			ss = ssh_connect( glue('{remoteuser}@{remotehost}'), passwd =  remotepwd)
			scp_upload(ss, path, to = dir, verbose = TRUE)
			ssh_disconnect(ss)
			}

	glue('{dir}/{table}.csv')	

 	}

