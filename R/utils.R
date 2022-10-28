
  #' @export
rw2base64 <- function(f, width = 500) {
  tf <- tempfile(fileext = ".png")
  darkt <- system(glue("darktable-cli --width {width} {f} {tf}"))

  on.exit(file.remove(tf))
  
  if(file.exists(tf))
    o = base64enc::dataURI(file = tf, mime = "image/png")

  
  if(!file.exists(tf)) # NA photo
    o = "data:image/png;base64,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"

  o

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