

#' @import methods RMariaDB config magrittr stringr glue data.table foreach future doFuture pushoverr

NULL




.onLoad <- function(libname, pkgname){


	dcf <- read.dcf(file=system.file("DESCRIPTION", package=pkgname) )
  packageStartupMessage(paste(pkgname, 'v.', dcf[, "Version"] ))

  x = try(config::get(), silent = TRUE)
  if(!inherits(x, 'list')) 
  packageStartupMessage("No config.yml file found")


	}


