#' @title         read SNB data
#' @description   read an SNB file as data.table-s (only transponder and Light barrier data are parsed).
#' @param         f path to the snb file
#' @return        a  \code{data.table} .
#' @author        MV
#' @export
#' @examples 
#' x = read_boxtxt(system.file('test_files_SNB', '80', 'BOX0080.TXT', package = 'SNB2'))

read_boxtxt <- function(f) {
  d = readRaw_v2(f = f)
  file_path = str_remove(f, getOption('path.to.raw_v2')) 

  d[, V := str_to_upper(V)]
  d = d[ str_detect(V, 'TRANSPONDER:|LBO:|LBI:')  ]

  d[, datetime_ := snbstring2date_v2(V) ]
  d[,   sensor_value := str_extract(V, 'TRANSPONDER:[ \\t]*([^\\n\\r]*)') ]
  d[is.na(sensor_value), sensor_value := str_extract(V, 'LB[IO]:[ \\t]*([OFF|ON]*)')  ]
  d[, sensor := str_extract(sensor_value, 'LB[IO]|TRANSPONDER') ]
  d[, sensor_value := str_remove(sensor_value, 'LB[IO]:|TRANSPONDER:')]
  d[, sensor_value := str_trim(sensor_value)]
  d[, sensor := str_sub(sensor, 1, 3) %>% str_to_lower]

  # flag garbage 
  d[ str_count(sensor_value) != 16 & sensor == 'tra', g := 1]
  d[ !sensor_value %in% c('ON', 'OFF') & sensor %in% c('lbi', 'lbo'), g := 1]
  #prop garbage (from the total of possibly good lines)
  pg = nrow(d[g == 1])/nrow(d)

  # final subset
  o = d[is.na(g), .(datetime_, sensor_value, sensor)]
  o[, path := file_path ]

  if(nrow(o) == 0) pg = 1

  # set attributes (set file_path too because of empty files)
  setattr(o, 'SNB2', data.frame(box= basename2box(f), path = file_path, garbage = pg) )

  o

  }




#' @title          txt Box files (txt) to db b000 tables pipeline
#' @param  cnf  configuration variables are obtained from an external file config file. 
#'         default to config::get().
#' @return        TRUE if all txt are updated
#' @export
#' @examples
#' \dontrun{
#'  require(dup)
#'  scidb_snbUpdater.b000 ()
#' }
#' 
scidb_snbUpdater.b000 <- function(cnf = config::get()) {
    

    Start = Sys.time()
    
        u    = cnf$host$dbadmin
        h    = cnf$host$name
        p    = paste0(cnf$dir$base, cnf$dir$snb)
        y    = year(Sys.Date())
        db   = cnf$db$snb
        pwd  = cnf$host$dbpwd
        bb   = cnf$db$snb_boxes

        con = dbConnect(RMariaDB::MariaDB(), user = u, password = pwd, host = h, dbname = db)
        on.exit(dbDisconnect(con))


    cat('db set to', dQuote(db) , '...OK\n' )

    cat(' ------> Searching for proper directory formats ...' )
        x = data.table( dirs = list.files( paste0(p, y), full.name = TRUE) )
        x[, dirnam := basename(dirs) ]
        if(nrow(x) == 0) {
            stop(p, 'does not have any files')
            cat(p, 'does not have any files' )
        }
        isNotDate = x[ ! grepl('^[0-9]{4}\\.[0-9]{1,2}\\.[0-9]{1,2}$', dirnam )   ]
        if( nrow(isNotDate) > 0) {
            cat('Invalid directories\n' )
            stop('Invalid directories')
            }

    cat(' ------> Getting the file listing on ', p , '...' )
        rawf = data.table( path = list.files(p, full.name = TRUE, recursive = TRUE) )
        rawf[, box := basename2int(path)]
        rawf = rawf[ box %in%  bb ]
    
        cat('got', nrow(rawf), 'raw files.\n' )

    cat(' ------> Getting files already on ', db , '.b000 ... '  )
        dbf = data.table(box = paste0("b", str_pad(bb, 3, "left", pad = "0")) )
        dbf[, sql :=  paste0('select distinct path FROM ', db, '.', box ) ]
        dbf = dbf[, dbGetQuery(con, sql) , by = box]

        cat('got', nrow(dbf), ' files listed in DB.\n' )
        
    cat(' ------> Getting black listed files '  )
        blf = dbGetQuery(con, paste0('select distinct path from ',db, '.black_list')  )
        cat('got', nrow(blf), 'black list files.\n' )

        alldbf = rbind(dbf[, .(path)], blf)
        alldbf[, indb := TRUE]
        alldbf[, path := paste0(p, path)]
   
    cat(' ------> Identifying new files ...' )
        if(nrow(alldbf) == 0) newf = copy(rawf) else {

        newf = merge(rawf, alldbf, by = 'path', all.x = TRUE)
        newf = newf[is.na(indb)]
        }

        if( nrow(newf) == 0 ) {
            cat('None found. Will stop now.\n' )
            return(0)
            stop ('-------- NO NEW FILES FOUND -------- ')

        } 

        cat('got', nrow(newf), 'new files. OK\n' )

    cat(' ------> Parsing new txt files ....'  )

        O = foreach(i = 1: nrow(newf)  )  %do% {
            read_boxtxt(newf[i,path])      
          } ; cat('OK\n' )

    cat(' ------> Find if there are black listed files ....' )
        B = lapply(O, function(x) attributes(x)$SNB2 )
        B = rbindlist(B)
        B = B[garbage > 0.5, .(path)]

        if(nrow(B) > 0) {
            cat('got', nrow(B), 'bad files. will write to black_list ... \n' )

        DBI::dbWriteTable(con, 'black_list' , B , row.names = FALSE, append = TRUE)

        } else cat('All files are OK... \n' )
        
    cat(' ------> Updating b000 tables on', dQuote(db),'.... ' )
        pb = txtProgressBar(max = length(O), style = 3 )
    
        out = foreach(i = 1: length(O), .combine = c, .final=sum)  %do% {
            oi = O[[i]]
            atr = attributes(oi)$SNB2

            if(atr$garbage < 0.5) {
              res = DBI::dbWriteTable(con, int2b(atr$box) , oi , row.names = FALSE, append = TRUE)
              } else res = FALSE
            setTxtProgressBar(pb, i)
            res
        } 
        
        cat('\n      Uploaded', out , 'new files.\n' )

    cat(' ------> Done in', timetaken(Start) )

    tt = difftime(Sys.time, Start, units = 'mins') %>% round %>% as.character

    o = c(tt, out)
    names(o) = c('Mins_taken', paste('N_files_uploaded to', db)  )
    o

 }



#' @title   b000 tables to transponders table pipeline
#' @param    cnf  configuration variables are obtained from an external file config file. 
#'             default to config::get().
#' @return  TRUE on success
#' @export
#' @examples
#' \dontrun{
#'  require(dup)
#'  
#'  scidb_snbUpdater.transponders()
#' }
#' 

scidb_snbUpdater.transponders <- function(cnf = config::get() ) {


    Start = Sys.time() 
    cat(' ------> Getting settings ...\n') 
        u    = cnf$host$dbadmin
        h    = cnf$host$name
        p    = paste0(cnf$dir$base, cnf$dir$snb)
        y    = year(Sys.Date())
        db   = cnf$db$snb
        pwd  = cnf$host$dbpwd
        bb   = cnf$db$snb_boxes
        tdb  = cnf$db$transponders

        con = dbConnect(RMariaDB::MariaDB(), user = u, password = pwd, host = h, dbname = db)
        on.exit(dbDisconnect(con))


        cat('db set to', dQuote(db), 'transponders db set to', dQuote(tdb) , '...OK\n')

    cat(' ------> Getting the file list on ', db , '.b000 ... ' )

        box000f = data.table(box = int2b(bb) )
        box000f[, sql :=  paste('select distinct path FROM', db, '.', box ) ]
        box000f = box000f[, dbGetQuery(con, sql), by = box]

        cat('got', nrow(box000f), ' files listed in DB.\n')


    cat(' ------> Getting the file list on ', tdb , '.transponders ... ', sep = '' )

        transpf = dbGetQuery(con, paste0('select distinct path from ', tdb, '.transponders') ) 
        setDT(transpf)
        transpf[, done := 1]

        cat('got', nrow(transpf) , ' files listed in transponders and ')

        newf = merge(box000f, transpf, by = 'path', all.x = TRUE)
        newf = newf[is.na(done)]
        cat(nrow(newf) , ' files to append to transponders ... \n')

    cat(' ------> Running INSERT INTO STATEMENTS for each b000 table ...')
        newf = newf[, .(path = paste( shQuote(path), collapse = ',') ), by = box]
        newf[, path := paste('(', path, ')')]
        newf[, boxno := b2int(box)]

        newf[, sql := paste(
            paste0('INSERT INTO ' , tdb, '.transponders' ,  ' (site_type, site, transponder,datetime_, path )'),
            "select 1 site_type,", boxno,  " site, sensor_value transponder, datetime_, path 
                from", box,
                "where sensor = 'tra' and path in ", path)]

        if( nrow(newf) > 0 )
        newf[, o := dbExecute(con, sql), by = box]

        cat(sum(newf$o) , 'rows inserted into transponders ... \n')



    tt = mins_taken(Start)

    o = c(tt, sum(newf$o) )
    names(o) = c('Mins_taken', paste0('N_rows_uploaded to ', tdb, '.', db)  )
    o




    }







