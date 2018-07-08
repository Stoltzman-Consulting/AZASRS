#' Get valdate
#'
#' Finds valdate from fundinfo table
#'
#' @param db_con database connection
#' @return valdate as character
#' @export
get_valdate = function(){
  db_con = DBI::dbConnect(drv = AZASRS_DATABASE_DRIVER, dbname = AZASRS_DATABASE_LOCATION)
  querystring = paste0("SELECT csdate FROM fundinfo")
  res = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(res)
  DBI::dbClearResult(res)
  DBI::dbDisconnect(db_con)
  valdate = dat[!is.na(dat)][1]
  return(valdate)
}
