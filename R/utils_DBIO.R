
#' Makes a database connection based on a config file
#' @export
#' @return a MariaDB connection object
mariacon <- function(db) {
   cnf = config::get()
   host <- cnf$host$name
   user <- cnf$host$dbadmin
   pwd <- cnf$host$dbpwd

   con <- try(
       DBI::dbConnect(RMariaDB::MariaDB(),
           user = user,
           password = pwd,
           host = host,
           dbname = db
       ),
       silent = TRUE
   )

   if(inherits(con, "try-error") ) { # try again through VPN
   Sys.setenv("R_CONFIG_ACTIVE" = "tailscale")

   host <- config::get()$host$name
   con <-
       DBI::dbConnect(RMariaDB::MariaDB(),
           user = user,
           password = pwd,
           host = host,
           dbname = db
       )
   } 
   con
}




#' mysqldump
#'
#' @param db        db
#' @param tables    tables are given as a "tableName1 tableName2".
#' @param user      user
#' @param pwd       pwd
#' @param host      default to '127.0.0.1'
#' @param filenam   filenam. If missing then constructed from Sys.time()
#' @param dir       saving location on disk.
#' @param dryrun    when TRUE return call only. default to FALSE.
#' @param compress  when TRUE archive the sql output. default to TRUE.
#' @param ...       further arguments to mysqldump 
#'                  (e.g. --no-data --no-create-db)
#'
#' @return    the file path to the sql file or system call 
#'            when dryrun = TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' fp = mysqldump('tests',  user = 'testuser', pwd = '', dir = tempdir(), filenam = "tempbk.sql" , dryrun = TRUE)
#' mysqldump('tests', 't1 t2', 'testuser', pwd = '', dir = tempdir(), filenam = "tempbk.sql" , dryrun = TRUE)
#' mysqldump('tests', 't1', 'testuser', pwd = '', dir = tempdir(), filenam = "tempbk.sql", compress = FALSE , dryrun = TRUE)
#' }
#'
#'
mysqldump <- function(db,tables,user, pwd, host = '127.0.0.1', filenam, dir = getwd(), 
    dryrun = FALSE, compress=TRUE, ...) {


    if (compress) {
        filenam = paste0(filenam, ".gz")
    }
            

    filepath = paste(dir, filenam, sep = .Platform$file.sep)


    syscall = paste0('mysqldump  --host=', host,
                ' --user=' ,     user,
                ' --password=' , pwd,
                ' --databases ',  db,
                if(!missing(tables))   paste(' --tables ', paste(tables, collapse = " ") ) else NULL ,
                ' --routines ',
                if(!compress) paste0(' --result-file=', filepath) else NULL,
                ' --default-character-set=utf8mb4 --max-allowed-packet=1073741824 --verbose --skip-comments', ...)

    if(compress)
        syscall = paste0(syscall, " | gzip >", filepath)

    if(dryrun)  { 
        cat(syscall, '\n-------')
        return(syscall)
    
        }

    if(!dryrun) system(syscall, wait = TRUE)

    cat('Output file:', filepath, '\n')
    cat('File size:', file.size(filepath), '\n')

    return(filepath)
    }



#' mysqldump_host
#' 
#' @param  cnf  configuration variables are obtained from an external file config file. 
#'         default to config::get().
#' @param exclude   db-s to exclude default to c('mysql', 'information_schema', 'performance_schema')
#' @export

#' @examples
#' \dontrun{
#' Sys.setenv(R_CONFIG_ACTIVE = "default")
#' dup::mysqldump_host()
#' 
#' }

mysqldump_host <- function(cnf = config::get(), exclude = c('mysql', 'information_schema', 'performance_schema', 'phpmyadmin') ) {
    

        host  = cnf$host$name
        user  = cnf$host$dbadmin
        pwd   = cnf$host$dbpwd
        bkdir = cnf$dir$backupdir


        con <- dbcon(server = "scidb")
        stopifnot(con@host == host)
        on.exit(dbDisconnect(con))


        # db listing
        x = dbq(con, "SELECT DISTINCT TABLE_SCHEMA db FROM information_schema.`TABLES`")[! db %in% exclude]



        # prepare dir locations 
        maindir = paste0(bkdir, '/backup_', host, '_', format(Sys.time(), "%d-%b-%Y-%HH"))
        if(dir.exists(maindir)) stop(bkdir, " directory exists!")

        dir.create(maindir, recursive = TRUE)
        dir.create(paste0(maindir, '/DATA'), recursive = TRUE)
        dir.create(paste0(maindir, '/USERS'), recursive = TRUE)


        x[, path := paste0(maindir, '/DATA/', db)]
        x[, dir.create(path), by = path]

    # DUMP data
        x[, i := .I]             
        x[, mysqldump(db = db, host = host, user = user, pwd = pwd, dir = path), by = i ]


    # DUMP mysql.global_priv (users and privileges)
        # users
        # mysqldump(db = 'mysql', tables = 'global_priv', host = host, user = user, pwd = pwd, dir = paste0(maindir, '/USERS') )
        # privileges
        con = dbConnect(RMariaDB::MariaDB(), user = user, password = pwd, host = host)

        x = dbGetQuery(con, "SELECT distinct User FROM mysql.user where user not in 
                                ('debian-sys-maint', 'root', 'phpmyadmin');")
        setDT(x)
        x = x[, dbGetQuery(con, glue("SHOW GRANTS FOR '{User}'@'%'") ), by = User]
        setnames(x, c('user', 'grants'))
        fwrite(x, paste0(maindir, '/USERS/grants.txt'))

        dbDisconnect(con)  


    # FEEDBACK

        tt = difftime(Sys.time(), started.at, units = 'mins') %>% round %>% as.character
        
        du = system(paste('du --max-depth=0 -B 1M', maindir), intern = TRUE) %>% 
                str_split('\\t', simplify = TRUE) %>%
                extract(1) %>% 
                as.numeric 
        du = round(du/1000,  3)


        nfiles = system(paste('find',  maindir , '-type f | wc -l') , intern = TRUE)

        smallestFile = system( glue("find {maindir}/DATA -type f| grep sql.gz | awk 'NR==1'"), intern = TRUE)
        size_smallestFile = file.size(smallestFile)*1e-6
        size_smallestFile = round(size_smallestFile, 1) %>% paste(., 'MB')    
        smallestFile = paste( basename(smallestFile), size_smallestFile )
        
        msg = paste(
            glue('{tt}  mins'), 
            glue('{nfiles} files'),
            glue('{du} GB'),
            glue('{smallestFile}'),
            sep = '\n')


        msg

    }




#' mysqlrestore
#'
#' restore sql file locally
#' @param file    sql or sql.gz file
#' @param db      database name
#' @param user    user, default to 'root'
#' @param host    default to  '127.0.0.1'
#' @param dryrun only print the mysql cli call and exit
#' @export
#'

mysqlrestore <- function(file, db, user, pwd , host =  '127.0.0.1', dryrun = FALSE) {



    if( !missing(db) &  !dryrun) {
        makedbcall = glue('echo "CREATE DATABASE IF NOT EXISTS {db}" | mysql -h{host} -u{user} -p{pwd}')
        system(makedbcall)
        }
    
    makedbcall = glue('echo "SET GLOBAL max_allowed_packet=1073741824" | mysql -h{host} -u{user} -p{pwd}')
    system(makedbcall)      

    mysqlCall =     glue('mysql  --max-allowed-packet=1073741824 --net_buffer_length=1000000 -h{host} -u{user} -p{pwd} {db}')


    if(tools::file_ext(file) == 'sql')
        syscall = paste(mysqlCall, '<', file )

    if(tools::file_ext(file) == 'gz')
        syscall = paste('gunzip -c', shQuote(file), "|", mysqlCall)

    if(dryrun)
        cat('\n----------\n', syscall, '\n----------\n')
    
    if(!dryrun)     
    system(syscall, wait = TRUE)

    }



#' mysqlrestore_host
#'
#' restore an entire db system or several db-s
#' @param  cnf            configuration variables are obtained from an external file config file. 
#'                          default to config::get().
#' @param  backup         path to backup dir. if missing the last backup is used.
#' @param  restore_users  restore mysql.user table
#' @param  parallel       default to TRUE
#' @param  exclude        db to exclude from restoring
#' @param  host_is_set    default to FALSE
#' @param  ...            further options passed to mysqlrestore
#' @export
#' 
#' 
#' @examples
#' \dontrun{
#' 
#'  require(dup)
#'  Sys.setenv(R_CONFIG_ACTIVE = "localhost")
#'  mysqlrestore_host()
#' }
#'
#'
mysqlrestore_host <- function(cnf = config::get(), backup,wipe = FALSE, 
    restore_users = FALSE, parallel = TRUE, exclude, host_is_set = FALSE) {

    # INI
        started.at=Sys.time()

        host  = cnf$host$name
        user  = cnf$host$dbadmin
        pwd   = cnf$host$dbpwd
        bkdir = cnf$dir$backupdir

        if(!host_is_set) {
            message('Are you sure ', host, ' is what you want?')
            stop('Set `host_is_set` to TRUE and try again')
            }




        if(missing(backup)) {
            x = data.table( p = list.dirs(bkdir, recursive = FALSE) )
            x[, dt := basename(p) %>% str_extract('\\d{1,2}-\\b[a-zA-Z]{3}\\b-\\d{4}-\\d{2}H') %>% anytime   ]
            backup = x[dt == max(dt, na.rm = TRUE), p]
        } else 
        backup = paste(bkdir, backup, sep = '/')

        message(paste('backup path is', backup))


        con = DBI::dbConnect(RMariaDB::MariaDB(), user = user, password = pwd, host = host)
    

        # db-s
        o = data.table(maindirs = list.dirs(backup, full.names = FALSE, recursive = FALSE) )
        if(!all( o$maindirs == c('DATA',  'USERS') ) ) stop('invalid backup directory.')


        # DATA dump file listing
        d = data.table(db_dumps = list.files(paste0(backup, '/DATA'), full.names = TRUE, recursive = TRUE) )
        d[, db := dirname(db_dumps) %>% basename]
    
        if(!missing(exclude)) {
            d = d[!db%in%exclude]
        }

        if(nrow(d) == 0) stop('Nothing to restore!')

    


    # INIT RESTORE DATABASES
        d[, isnewdb := DBI::dbExecute(con, paste('CREATE DATABASE', db)), by = db]

    # Restore DATA
        if(parallel) {
            DBI::dbExecute(con, "SET GLOBAL max_connections = 300;")
            doFuture::registerDoFuture()
            future::plan(future::multiprocess)
            }

        dbDisconnect(con)

        foreach( i = 1:nrow(d) )  %dopar% {

            d[i, mysqlrestore(file = db_dumps, db = db, host = host, user = user, pwd = pwd) ]

            }


    # Restore USERS 
        if(restore_users) {
            con = DBI::dbConnect(RMariaDB::MariaDB(), user = user, password = pwd, host = host)
    
            x = fread(paste0(backup, '/USERS/grants.txt'))
            x[, id := .I]
            x[, o := try(DBI::dbExecute(con, grants) %>% as.character, silent = TRUE) , by = id]
            print(x[o != '0'])

            DBI::dbExecute(con, 'FLUSH PRIVILEGES')
            
            dbDisconnect(con)

            }   


    # return time taken

        difftime(Sys.time(), started.at, units = 'mins')


 }


#' rm_old_backups
#'
#' remove ond backups
#' @param  path  path to backup dir. default taken from the config.yml file retrieved by 
#' @param  keep  how many prior backups to keep
#' @return names of removed backups
#' @export
rm_old_backups <- function(path = config::get('dir')$backupdir , keep = 10) {
    

    x = data.table( p = list.dirs(path, recursive = FALSE) )
    
    x[, dt := basename(p) |> stringr::str_extract("\\d{1,2}-\\b[a-zA-Z]{3}\\b-\\d{4}-\\d{2}H")]
    x[, dt := anytime::anytime(dt)]
 
    x = x[!is.na(dt)]
    x[, i := .I]

    x = x[, dirSize := dir_size(p), by = i]
    x = x[dirSize > 0]

    setorder(x, -dt)
    x[, i := .I]
    x[, remove := i > keep]
    x[(remove), removed := fs::dir_delete(p)]

    x[!is.na(removed), removed]

    }



#' txtdump
#' @param  db     db name
#' @param  table  table name
#' @param  dir    directory path
#' @param  remote when TRUE the file is uploaded to a remote host defined in cnf
#' @param  cnf  configuration variables (host, user, pwd, remotehost) are obtained
#' 				from an external file config file. default to config::get().
#'
#' @return path of the dumped file
#' @export
#'
#' @examples
#' \dontrun{
#'
#' require(dup)
#' Sys.setenv(R_CONFIG_ACTIVE = "localhost")
#' txtdump(db = "ARGOS", table = "2019_LBDO")
#' }
#'
txtdump <- function(db, table, remote = TRUE, dir = ".", cnf = config::get()) {
    host <- cnf$host$name
    user <- cnf$host$dbadmin
    pwd <- cnf$host$dbpwd
    remoteuser <- cnf$remotehost_1$dbadmin
    remotepwd <- cnf$remotehost_1$syspwd
    remotehost <- cnf$remotehost_1$name

    con <- dbConnect(RMariaDB::MariaDB(),
        user = user, password = pwd,
        host = host, db = db
    )
    on.exit(dbDisconnect(con))

    x <- dbReadTable(con, table)
    setDT(x)

    if (!remote) {
        path <- glue("{dir}/{table}.csv")
        fwrite(x, path, yaml = TRUE)
    }

    if (remote) {
        path <- glue("{tempdir()}/{table}.csv")
        fwrite(x, path, yaml = TRUE)
        ss <- ssh_connect(glue("{remoteuser}@{remotehost}"), passwd = remotepwd)
        scp_upload(ss, path, to = dir, verbose = TRUE)
        ssh_disconnect(ss)
    }

    glue("{dir}/{table}.csv")
}





#' db_copy
#' copy from one host to another. should be run from 'dst'.
#' @param  src source host
#' @param  dst destination host
#' @param  cnf  configuration variables are
#'              obtained from an external config file.
#'              default to config::get().
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' require(dup)
#' Sys.setenv(R_CONFIG_ACTIVE = "localhost")
#' db_copy("FIELD_BTatWESTERHOLZ", "remotehost_1", "host")
#' }


#'
db_copy <- function(db, src, dst, cnf = config::get()) {
    # settings
    dst_host   <- cnf[dst][[1]]$name
    dst_dbuser <- cnf[dst][[1]]$dbadmin
    dst_dbpwd  <- cnf[dst][[1]]$dbpwd

    src_host   <- cnf[src][[1]]$name
    src_dbuser <- cnf[src][[1]]$dbadmin
    src_dbpwd  <- cnf[src][[1]]$dbpwd
    src_syspwd <- cnf[src][[1]]$syspwd


    # get dump from remote
    ss <- ssh_connect(glue("{src_dbuser}@{src_host}"), passwd = src_syspwd)
    mycall <- mysqldump(
        db = db, user = src_dbuser, pwd = src_dbpwd,
        dir = glue("/home/{src_dbuser}"),
        dryrun = TRUE, compress = FALSE
    )
    ssh_exec_wait(ss, mycall)
    scp_download(ss, glue("/home/{src_dbuser}/{db}.sql"), to = tempdir())
    ssh_disconnect(ss)

    # upload dumped file to database
    path <- glue("{tempdir()}/{db}.sql")
    mysqlrestore(path, db, dst_dbuser, dst_dbpwd, dst_host)
    file.remove(path)
}
