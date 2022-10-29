

#' @import methods utils stats RMariaDB RSQLite data.table
#' @import magrittr stringr glue foreach future doFuture pushoverr  anytime crayon
#' @import fs ssh
#' @import magick
#' @importFrom config get 
#' @importFrom geodist geodist

NULL


.onLoad <- function(libname, pkgname) {

	dcf <- read.dcf(file=system.file("DESCRIPTION", package=pkgname) )
  packageStartupMessage(paste(pkgname, 'v.', dcf[, "Version"] ))

  x = try(config::get(), silent = TRUE)
  if(!inherits(x, 'list')) {
    packageStartupMessage("Config.yml does not exist.")
  }
  else {
     packageStartupMessage("Config.yml found!")
  }

}
