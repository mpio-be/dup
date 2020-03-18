


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
#'  require(dup)
#'  Sys.setenv(R_CONFIG_ACTIVE = "localhost")
#'  db_copy("FIELD_BTatWESTERHOLZ", "remotehost_1", "host")

#' }
#' 
db_copy <- function(db, src, dst,cnf = config::get() ) {
    # settings
        dst_host    = cnf[dst][[1]]$name
        dst_dbuser  = cnf[dst][[1]]$dbadmin
        dst_dbpwd   = cnf[dst][[1]]$dbpwd

        src_host    = cnf[src][[1]]$name
        src_dbuser  = cnf[src][[1]]$dbadmin
        src_dbpwd   = cnf[src][[1]]$dbpwd
        src_syspwd  = cnf[src][[1]]$syspwd


    # get dump from remote
        ss = ssh_connect( glue('{src_dbuser}@{src_host}'), passwd =  src_dbpwd)
        mycall = mysqldump(db=db,user=src_dbuser, pwd=src_dbpwd, 
                           dir = glue('/home/{src_dbuser}') , 
                           dryrun = TRUE, compress = FALSE)
        ssh_exec_wait(ss, mycall)
        scp_download(ss, glue('/home/{src_dbuser}/{db}.sql'), to=tempdir())
        ssh_disconnect(ss)    

    # upload dumped file to database
        path = glue('{tempdir()}/{db}.sql')
        mysqlrestore( path, db, dst_dbuser, dst_dbpwd, dst_host)



    }

