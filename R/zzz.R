

#' @import methods utils stats RMariaDB magrittr stringr glue foreach future doFuture pushoverr data.table anytime crayon ssh
#' @import fs ssh
#' @importFrom config get 
#' @importFrom geodist geodist

NULL




.onLoad <- function(libname, pkgname) {


	dcf <- read.dcf(file=system.file("DESCRIPTION", package=pkgname) )
  packageStartupMessage(paste(pkgname, 'v.', dcf[, "Version"] ))

  x = try(config::get(), silent = TRUE)
  if(!inherits(x, 'list')) 
  packageStartupMessage("No config.yml file found")


	}
