#' @export
mins_taken <- function(x) {
  assert_that( is.time(x) )
  o = difftime(Sys.time(), x, units = 'mins') %>% round(digits = 1)
  glue('{o} minutes taken.')
}


#' @export
push_msg <- function(x, title, cnf = config::get('pushover') ) {
   x = paste(x, collapse = ' ')
   pushoverr::pushover(message = x, title = title, user = cnf$user, app = cnf$app)
  
  }



#' @export
owncloud <- function(dir = "path_relative_to_ownCloud",user, pass, exclude = c("*.sublime-workspace") , dryrun = FALSE, reset = FALSE) {

    locDir = str_glue('~/ownCloud/{dir}')
    system(str_glue('mkdir -p {locDir}'))


    # make exclude file
    exf = '~/.owncloud_exclude.lst'
    writeLines(exclude, exf)


    # cmd
    if(reset) rm(.__owncloudcmd__, envir = .GlobalEnv)

    cmd = get0('.__owncloudcmd__', envir = .GlobalEnv)

    if(is.null(cmd) ) {
    cmd = str_glue(
      "owncloudcmd  --nonshib --user {user} --password {shQuote(pass)} --exclude {exf} {locDir} https://owncloud.gwdg.de/remote.php/nonshib-webdav/{dir}")

    assign('.__owncloudcmd__', cmd, envir = .GlobalEnv)


    }

    cmd = base::get('.__owncloudcmd__', envir = .GlobalEnv)



    if(!dryrun)
    system(cmd)

    if(dryrun)
    cat(cmd)


}




#' argosfilenam2date
#' @export
argosfilenam2date <- function(x, sepDate = "") {
  if(sepDate == "") {
    s = str_extract(x , '(20\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})')
    o = strptime(s, "%Y%m%d%H%M%S")
    }

  if(sepDate == "-") {
    s = str_extract(x ,'(20\\d{2})-(\\d{2})-(\\d{2})')
    o = anytime(s)
    }

    o

 }



 #' sqlin
#' @description prepare string for select ... where in (1,2,3) 
#' @param       s     char vector
#' @export
#' @examples
#' sqlin( 1:3)

sqlin <- function(s) {

  paste(s, collapse = ',') %>% 
  paste0('(', . , ')')

}


#' @export
basename2int <- function(ff) {
  basename(ff) %>% 
   str_extract("-?\\d+") %>%
   as.integer
  }


#' @export
int2b <- function(x) {
    paste0("b", str_pad(x, 3, "left", pad = "0"))
    }

#' @export
b2int <- function(x) {
  str_remove(x, 'b') %>%
  as.integer
  }