# ==========================================================================
# DATA SENT BY DIFFERENT PROVIDERS THROUGH EMAIL
# 1. extract_email_attachements() calls offlineimap() and extracts data from 
#    new emails '/home/mihai/incoming_data/email_attachments/'; it runs on CRON
# 2. read_email_attachements() it is used for DB update; it runs on CRON
#    and updates files not yet on DB
# ==========================================================================

#' offlineimap
#' @param  accounts      accounts       
#' @param  localfolders  localfolders   
#' @param  remotehost    remotehost     
#' @param  remoteuser    remoteuser     
#' @param  maildir       maildir     
#' @param  pwd           pwd            
 
#' @return NULL
#' @export
#' @examples
#' pwd = sdb::getCredentials('gitlab',host = 'gwdg')$pwd
#' offlineimap(pwd = pwd, maildir = 'ARGOS')
#' offlineimap(pwd = pwd, maildir = 'GSM_MTI')


offlineimap <- function(accounts = 'gitlab', localfolders = '/ds/raw_data_kemp/FIELD/ARGOS/incoming_data/email/',
remotehost="email.gwdg.de",remoteuser = "gitlab-grpkempenaers@orn.mpg.de",pwd, maildir) {

    tf = tempfile(); on.exit( file.remove(tf) )
    Cat = function(...) { cat(..., file = tf, sep = '', append = TRUE, fill = TRUE)}

    # TEMP CONFIG FILE 
    localrepository = paste0(accounts, 'loc')
    remoterepository = paste0(accounts, 'rem')

    Cat('[general]' )
      Cat('accounts=', accounts)

    Cat('[Account ', accounts, ']')
      Cat('localrepository=', localrepository)
      Cat('remoterepository=', remoterepository)

    Cat('[retriever]' )
      Cat('type=SimpleIMAPSSLRetriever' )

    Cat('[Repository ', localrepository, ']')
      Cat('type=Maildir' )
      Cat('localfolders=', localfolders )

    Cat('[Repository ', remoterepository, ']' )
      Cat('type=IMAP' )
      Cat('remotehost=', remotehost )
      Cat('remoteuser=', remoteuser )
      Cat('remotepass=', pwd )
      Cat('ssl=yes')
      Cat('sslcacertfile=/etc/ssl/certs/ca-certificates.crt')

     
     # file.edit(tf)

    call = paste('offlineimap -f', shQuote(paste0('INBOX/', maildir)), '-c', tf)  
    system(call)
    }



