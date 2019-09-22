


#' @title Argos incoming
#' @param  cnf  configuration variables are obtained from an external file config file. 
#'         default to config::get().
#' @param user default mihai
#' @param daysBefore converted to  Sys.Date() - 365 and passed to read_email_attachements(lastdate=). 
#' @export
#' @examples
#' scidbupdate_ARGOS.incoming()
scidbupdate_ARGOS.incoming <- function(cnf = config::get(), daysBefore = 365 ) {

		host = cnf$host$name
		user = cnf$host$dbadmin
		pwd  = cnf$host$dbpwd


		from = as.POSIXct(Sys.Date() - 365)

		con = dbConnect(RMariaDB::MariaDB(), user = user, password = pwd, host = host)
   	    on.exit(dbDisconnect(con))

		P = dbGetQuery(con, 'select tableName FROM projects WHERE active = "Y"')
		P = rbind(P, data.frame(tableName = 'incoming'))
		done = P[, dbGetQuery(con, paste('select distinct filenam from', tableName) ) , by = tableName ]



		# already uploaded to db 

		if(nrow(done) > 0 )
		x = read_email_attachements(maildir='ARGOS', sep = ";", lastdate = from, exclude = done$filenam , sepDate = "")

		if(nrow(done) == 0 )
		x = read_email_attachements(maildir='ARGOS', sep = ";", lastdate = from, sepDate = "")


		if(nrow(x) > 0) {

			x = x[!is.na(PTT)]

			# write to DB
			setnames(x, make.names(names(x)))

			x = x[, .(PTT, Satellite, Location.date, Message.date, Location.class, Compression.index, Latitude, Longitude, X1, X2, X3, X4, X5, X6, X7, X8, filenam)]

			setnames(x, c("tagID", "satellite", "locationDate", "messageDate", "locationClass", "compressionIndex", "latitude", "longitude", "S1","S2","S3","S4","S5","S6","S7","S8", "filenam") )

			# announce last pk
			lpk = dbq(con, 'select max(pk) pk from incoming')$pk
			message(paste('----------> last pk in incoming = ', lpk))

			return(dbWriteTable(con, 'incoming', x, row.names = FALSE, append = TRUE))

			} else FALSE
	
}




#' @title Argos: move from incoming to YYYY_SSSS
#' @param  cnf  configuration variables are obtained from an external file config file. 
#'         default to config::get().
#' @export
#' @examples
#' scidbupdate_ARGOS.flush_incoming()
scidbupdate_ARGOS.flush_incoming <- function(cnf = config::get() ) {
	
	host = cnf$host$name
	user = cnf$host$dbadmin
	pwd  = cnf$host$dbpwd

	con = dbConnect(RMariaDB::MariaDB(), user = user, password = pwd, host = host)
	on.exit(dbDisconnect(con))

	P = dbGetQuery(con, 'select JSON_COMPACT(tagIDs) tagIDs, startDate, tableName 
					FROM projects  WHERE active = "Y" ')

	# find pk-s in incoming not yet in YYYY_SSSS tables
	P[, tagIDs := str_replace(tagIDs, '\\[', "(")]
	P[, tagIDs := str_replace(tagIDs, '\\]', ")")]

	P[, q := paste('SELECT pk from incoming WHERE locationDate >=', shQuote(startDate), 'and tagID in', tagIDs )]

	x = P[, dbGetQuery(con, q), by = tableName]
	x = x[, .(hot = sqlin(pk), n = .N ), by = tableName]
	
	x[, colnams :=  paste(setdiff(dbColumnInfo(con, 'incoming')$name, 'pk') , collapse = ',') ]

	x[, q := paste('INSERT INTO', tableName, '(', colnams, ') 
						SELECT ', colnams, 'FROM incoming 
								WHERE pk in',  hot )]
	# RUN
	x[n > 0, run := as.character(try(dbExecute(con, q), silent = TRUE)) , by = tableName]

	# when RUN ok then remove entries in incoming
	z = x[as.numeric(run) > 0, .(tableName, hot)]

	z[, q := paste('DELETE FROM incoming where pk in', hot)]
	
	if(nrow(z) > 0) {
		z[, run := dbExecute(con, q) , by = tableName]
		z[, .(run, tableName)]
	} else 
	FALSE

	}




#' @title Argos pipeline
#' @export
#' @import crayon
#' @examples
#' scidbupdate_ARGOS.pipeline()
scidbupdate_ARGOS.pipeline <- function() {

	cat( red$bold('\n ----> Get new emails and extract attachments ......\n') )
	extract_email_attachements(maildir="ARGOS")
	
	cat( blue$bold('\n ----> Read email attachments and update incoming table.....\n') )
	scidbupdate_ARGOS.incoming()

	cat( green$bold('\n ----> Distribute data from incoming table to YYYY_SPECIES table.\n') )
	o = scidbupdate_ARGOS.flush_incoming()

	o


	}

























