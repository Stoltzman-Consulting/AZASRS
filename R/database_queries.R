#' @importFrom magrittr %>%
#' @import DBI
#' @import RSQLite

# Global variables
#' @export
#AZASRS_DATABASE_LOCATION = "C:\\Users\\scotts\\Documents\\GitHub\\DB_Application\\asrs_temporary.db"
AZASRS_DATABASE_LOCATION = "P:\\IMD\\2018 Database Project\\asrs_temporary_db.db"

#' @export
AZASRS_DATABASE_DRIVER = RSQLite::SQLite()


#' Query cashflow data
#'
#' SELECT statement to retrieve all cashflow data
#'
#' @param db_con database connection
#' @return cashflow dataframe (date, shortname, amount)
#' @export
pull_cashflow = function(){
  db_con = DBI::dbConnect(drv = AZASRS_DATABASE_DRIVER, dbname = AZASRS_DATABASE_LOCATION)
  querystring = paste0("SELECT * FROM cashflow")
  res = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(res)
  DBI::dbClearResult(res)
  DBI::dbDisconnect(db_con)
  return(dat)
}


#' Query nav data
#'
#' SELECT statement to retrieve all nav data
#'
#' @param db_con database connection
#' @param from_date starting date ( >= ) at which data must be more recent than
#' @return nav dataframe (date, shortname, amount)
#' @export
pull_nav = function(){
  db_con = DBI::dbConnect(drv = AZASRS_DATABASE_DRIVER, dbname = AZASRS_DATABASE_LOCATION)
  querystring = paste0("SELECT * FROM nav")
  res = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(res)
  DBI::dbClearResult(res)
  DBI::dbDisconnect(db_con)
  return(dat)
}


#' Query fundinfo data
#'
#' SELECT statement to retrieve all fundinfo data
#'
#' @param db_con database connection
#' @return fundinfo dataframe columns requested back **TODO: currently retrieves all, need to narrow this down... should it be user feedback?
#' @export
pull_fundinfo = function(){
  db_con = DBI::dbConnect(drv = AZASRS_DATABASE_DRIVER, dbname = AZASRS_DATABASE_LOCATION)
  querystring = paste0("SELECT * FROM fundinfo")
  res = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(res)
  DBI::dbClearResult(res)
  DBI::dbDisconnect(db_con)
  return(dat)
}


#' Query benchmark data
#'
#' SELECT statement to retrieve all historical_mv data
#'
#' @param db_con database connection
#' @return dataframe of shortname, longname, date, price (adds in log price of ODCE as Fixed8)
#' @export
pull_benchmark = function(){
  db_con = DBI::dbConnect(drv = AZASRS_DATABASE_DRIVER, dbname = AZASRS_DATABASE_LOCATION)
  querystring = paste0("SELECT * FROM benchmark")
  res = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(res)
  DBI::dbClearResult(res)

  ### INSERT functions to add log, exp, days365 etc?

  DBI::dbDisconnect(db_con)
  return(dat)
}

