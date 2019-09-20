#' extract_email_attachements to their dirs AFTER calling offlineimap
#' @param maildir maildir
#' @param pwd    password
#' @param basesourcedir 
#' @param basetargetdir 
#' @param onlynew default to TRUE
#' @param keep default to ".*\\.(TXT|txt)"
#' @return NULL
#' @export
#' @examples
#' extract_email_attachements(maildir = 'ARGOS', pwd = sdb::getCredentials('gitlab',host = 'gwdg')$pwd, onlynew = TRUE)
#' extract_email_attachements(maildir = 'GSM_MTI', pwd = sdb::getCredentials('gitlab',host = 'gwdg')$pwd, onlynew = TRUE)
#' 
#' 

extract_email_attachements <- function(maildir, pwd, 
  basesourcedir = "/ds/raw_data_kemp/FIELD/ARGOS/incoming_data/email/", 
  basetargetdir = '/ds/raw_data_kemp/FIELD/ARGOS/incoming_data/email_attachments/', 
  onlynew = TRUE, keep =  ".*\\.(TXT|txt|csv|CSV)" ) {

  sourcedir = paste0(basesourcedir, paste0('INBOX.', maildir))
  targetdir = paste0(basetargetdir, maildir)

  dir.create(targetdir ,recursive = TRUE, showWarnings = FALSE )

  # offlineimap [ fetch new e-mails]
    t0 = data.table( path = list.files(path = sourcedir, full.names = TRUE, recursive = TRUE), new = 0  )
    
    offlineimap(pwd = pwd, maildir = maildir)

    t1 = data.table( path = list.files(path = sourcedir, full.names = TRUE, recursive = TRUE)  )

    o = merge(t0, t1, by = 'path', all.y = TRUE)
    if(onlynew)  
      o = o[is.na(new)]


  # munpack [ extract attachments ]  
    o[, munpack_call := paste('munpack -f -C',targetdir, path) ]
    if(nrow(o) > 0)
    o[ , system(munpack_call), by = path]


  # unzip
      a = data.table( path = list.files(path = targetdir, full.names = TRUE, recursive = TRUE) )
      zf = a[str_detect(path, ".zip$")]
      if(nrow(zf) > 0)
      zf[, unzip(zipfile = path, junkpaths = TRUE, exdir = targetdir), by = path]


  # cleanup unwanted files 
      g = list.files(path = targetdir, full.names = TRUE, recursive = TRUE) 
      g = g [ ! str_detect(g, keep) ]
      sapply(g, file.remove)


    

  }



#' read_attachements
#' @param maildir  maildir
#' @param basedir  default is set
#' @param exclude  exclude filenames (if they are in the db already)
#' @param lastdate date of the last included file (date is embedded in the filename)
#' @param pattern  regexp on file name.
#' @param ...     passed to fread
#' @return DT
#' @export
#' @examples
#' x = read_email_attachements(maildir='ARGOS'  , sep = ";")
#' x = read_email_attachements(maildir='GSM_MTI', pattern = "g_")
#' x = read_email_attachements(maildir='GSM_MTI', pattern = "e_")

read_email_attachements <- function(
  maildir, basedir =  '/ds/raw_data_kemp/FIELD/ARGOS/incoming_data/email_attachments/', 
  exclude, include, pattern,lastdate,sepDate = "", ...) {

   F = list.files( paste0(basedir, maildir), full.names = TRUE)
   if(!missing(pattern)  && length(F) > 0 )  F = F[ str_detect(basename(F), pattern = pattern)] 
   if(!missing(exclude)  && length(F) > 0 )  F = F[ ! basename(F) %in% exclude] 
   if(!missing(lastdate) && length(F) > 0 )  F = F[ argosfilenam2date(basename(F), sepDate = sepDate) >= lastdate]

   F = na.omit(F)


   if(length(F) > 0 ) {
     o = foreach(f = F)  %do% {
       x = fread(f, fill = TRUE, header = TRUE, ... )
       x[, filenam := basename(f)]
       x
     }
     
     return(rbindlist(o, fill = TRUE))
     
   } else data.table()

  }



#' @title Argos incoming
#' @param host  default to getOption('host')
#' @param user default mihai
#' @param daysBefore converted to  Sys.Date() - 365 and passed to read_email_attachements(lastdate=). 
#' @export
#' @examples
#' scidbupdate_ARGOS.incoming()
scidbupdate_ARGOS.incoming <- function(host = getOption('host'), user = 'mihai', daysBefore = 365 ) {

		from = as.POSIXct(Sys.Date() - 365)
		con = sdb::dbcon(user = user, host = host, db = 'ARGOS'); on.exit(dbDisconnect(con))

		P = dbq(con, 'select tableName FROM projects WHERE active = "Y"')
		P = rbind(P, data.frame(tableName = 'incoming'))
		done = P[, dbq(con, paste('select distinct filenam from', tableName), enhance = FALSE ) , by = tableName ]



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
#' @export
#' @examples
#' scidbupdate_ARGOS.flush_incoming()
scidbupdate_ARGOS.flush_incoming <- function(host = getOption('host'), user = 'mihai' ) {
	
	con = sdb::dbcon(user = user, host = host, db = 'ARGOS'); on.exit(dbDisconnect(con))

	P = dbq(con, 'select JSON_COMPACT(tagIDs) tagIDs, startDate, tableName 
					FROM projects  WHERE active = "Y" ')

	# find pk-s in incoming not yet in YYYY_SSSS tables
	P[, tagIDs := str_replace(tagIDs, '\\[', "(")]
	P[, tagIDs := str_replace(tagIDs, '\\]', ")")]

	P[, q := paste('SELECT pk from incoming WHERE locationDate >=', shQuote(startDate), 'and tagID in', tagIDs )]

	x = P[, dbq(con, q), by = tableName]
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
	extract_email_attachements(maildir="ARGOS",pwd=getCredentials("gitlab",host="gwdg")$pwd)
	
	cat( blue$bold('\n ----> Read email attachments and update incoming table.....\n') )
	scidbupdate_ARGOS.incoming()

	cat( green$bold('\n ----> Distribute data from incoming table to YYYY_SPECIES table.\n') )
	o = scidbupdate_ARGOS.flush_incoming()

	o


	}

























