

#' @import methods utils RMariaDB magrittr stringr glue foreach future doFuture pushoverr data.table anytime crayon
#' @importFrom config get 

NULL




.onLoad <- function(libname, pkgname){


	dcf <- read.dcf(file=system.file("DESCRIPTION", package=pkgname) )
  packageStartupMessage(paste(pkgname, 'v.', dcf[, "Version"] ))

  x = try(config::get(), silent = TRUE)
  if(!inherits(x, 'list')) 
  packageStartupMessage("No config.yml file found")


	}


