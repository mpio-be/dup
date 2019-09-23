

#' @export
installCrontab <- function() {
  newtab = system.file('sysSet', 'crontab', package = 'dup')
  call = paste('cat ',newtab,' | crontab -')
  system(call, intern = TRUE)

  message('New crontab installed with\n', call)

  }



#' @export
owncloud <- function(dir = "path_relative_to_ownCloud",pass = pwd(), exclude = c("*.sublime-workspace") , dryrun = FALSE, reset = FALSE) {



  locDir = str_glue('~/ownCloud/{dir}')
  system(str_glue('mkdir -p {locDir}'))
  

  user = 'valcu@orn.mpg.de'

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

  cmd = get('.__owncloudcmd__', envir = .GlobalEnv)



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
int2b <- function (x) {
    paste0("b", str_pad(x, 3, "left", pad = "0"))
    }
