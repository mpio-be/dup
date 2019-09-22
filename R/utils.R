

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


