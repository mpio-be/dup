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

#' @export
snbstring2date_v2 <- function (x) {
    o = str_extract(x, "(^20\\d{2})(\\d{2})(\\d{2})-(\\d{6})\\.(\\d{3})")
    if (any(is.na(o))) 
        o = str_extract(x, "(^20\\d{2})(\\d{2})(\\d{2})-(\\d{6})")
    strptime(o, "%Y%m%d-%H%M%OS") %>% as.POSIXct
  }

#' @export
speed_along <- function(x, .lat = "latit", .lon = "longit", .dt = "datetime_", .grp = "tagID", clean = TRUE) {
  setnames(x, c(.lat, .lon, .dt, .grp), c(".lat", ".lon", ".dt", ".grp"))
  setorder(x, .dt, .grp)
  x[, .deltaT := difftime(.dt,
    shift(.dt, type = "lag"),
    units = "hour"
  ), by = .(.grp)]

  x[, .dst := geodist::geodist(cbind(.lat, .lon), sequential = TRUE, pad = TRUE, measure = "cheap"),
    by = .grp
  ]

  x[, speed_kmh := (.dst / 1000) / (.deltaT %>% as.numeric())]

  setnames(x, c(".lat", ".lon", ".dt", ".grp"), c(.lat, .lon, .dt, .grp))

  if (clean) {
    x[, ":="(.dst = NULL, .deltaT = NULL)]
  }
}