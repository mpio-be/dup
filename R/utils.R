
  #' @export
rw2base64 <- function(f, width = 500) {
  tf <- tempfile(fileext = ".png")
  darkt <- system(glue("darktable-cli --width {width} {f} {tf}"))



}

#' @export
expand_string <- function(x) {
  o <- str_replace(x, "\\-", ":")
  o <- glue("c({o})")
  o <- try(parse(text = o) |> eval(), silent = TRUE)
  if (inherits(o, "try-error")) o <- as.integer(NA)
  as.integer(o)
}


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
dir_listing <- function(dr) {
  x = data.table(path = list.files(dr, recursive = TRUE, full.names = TRUE))
  o = x[, fs::file_info(path)] |> setDT()
  o = o[, .(path, size = as.character(size), modification_time, birth_time)]
  out = paste0(str_remove(dr, "\\/$"), "_file_listing.csv")
  fwrite(o, file = out)
  out
  
}


#' @export
dir_size <- function(dr) {
  ff = list.files(dr, all.files = TRUE, recursive = TRUE, full.names = TRUE)
  o = do.call(rbind, lapply(ff, fs::file_info))
  sum(o$size)

  }