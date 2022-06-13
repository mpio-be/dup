

#' @export
accnamo_read_raw <- function(fnam) {
  d <- fread(fnam, colClasses = c("character", rep("numeric", 3)))
  bnam <- basename(fnam)
  setnames(d, "time", "datetime_")

  d[, tag_id := str_extract(bnam, "(?<=140000)(.*)(?=-HUB)")]
  d[, filenam := bnam]

  d
}


#' @export
accnano_2db <- function(dr, cnf = config::get() ) {
  
  host <- cnf$host$name
  user <- cnf$host$dbadmin
  pwd <- cnf$host$dbpwd

  con <- DBI::dbConnect(RMariaDB::MariaDB(), user = user, password = pwd, host = host, dbname = "FIELD_2022_CHARADRIIatBARROW")
  on.exit(dbDisconnect(con))


  canwrite <- dbGetQuery(con, " SHOW OPEN TABLES WHERE `Table` = 'ACC_NANO' ")$In_use == 0

  if (canwrite) {
    ff <- data.table(fnam = list.files(dr, full.names = TRUE, recursive = TRUE, pattern = ".csv"))
    ff[, bnam := basename(fnam)]

    dbff <- dbGetQuery(con, "SELECT distinct filenam FROM ACC_NANO") |> setDT()

    ff <- ff[!bnam %in% dbff$filenam]

    o <- foreach(i = 1:nrow(ff)) %do% {
      print(i)
      oi <- accnamo_read_raw(ff[i, fnam])
      dbWriteTable(con, "ACC_NANO", oi, row.names = FALSE, append = TRUE)
    }
  }
}
