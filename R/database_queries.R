#' @importFrom magrittr %>%

# Global variables
#' @export
AZASRS_DATABASE_LOCATION = "P:\\IMD\\2018 Database Project\\asrs_temporary_db.db"

#' @export
AZASRS_DATABASE_DRIVER = RSQLite::SQLite()


#' Connect to the database
#'
#' This function connects to a SQLite database (this should be changed later when production is ready) but the functionality will still apply
#'
#' @param db_driver is the driver for the type of database being accessed
#' @param db_location is the name of the database
#' @return A connection to the database
#' @export
db_connect = function(){
  con = DBI::dbConnect(drv = AZASRS_DATABASE_DRIVER, dbname = AZASRS_DATABASE_LOCATION)
  return(con)
}


#' Disconnect from the database
#'
#' This function connects to a SQLite database (this should be changed later when production is ready) but the functionality will still apply
#'
#' @param con is the current connection (likely db_connect)
#' @return Disconnect from the database
#' @export
db_disconnect = function(con){
  DBI::dbDisconnect(con)
}


#' Query cashflow data
#'
#' SELECT statement to retrieve all cashflow data
#'
#' @param db_con database connection
#' @return cashflow dataframe (date, shortname, amount)
#' @export
pull_cashflow = function(db_con = db_connect()){
  querystring = paste0("SELECT * FROM cashflow")
  rs = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(rs)
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
pull_nav = function(db_con = db_connect(), from_date='2018-01-01'){
  querystring = paste0("SELECT * FROM nav WHERE date >= '", from_date,"'")
  rs = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(rs)
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
pull_fundinfo = function(db_con = db_connect()){
  querystring = paste0("SELECT * FROM fundinfo")
  rs = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(rs)
  DBI::dbDisconnect(db_con)
  return(dat)
}


#' Query category data
#'
#' SELECT statement to retrieve all catetory data
#'
#' @param db_con database connection
#' @return category dataframe **TODO: way too much info, need to narrow this down to important stuff
#' @export
pull_categories = function(db_con = db_connect()){
  querystring = paste0("SELECT * FROM category")
  rs = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(rs)
  DBI::dbDisconnect(db_con)
  return(dat)
}


#' Query historical market value data
#'
#' SELECT statement to retrieve all historical_mv data
#'
#' @param db_con database connection
#' @return dataframe of shortname, date, amount
#' @export
pull_historicalmv = function(db_con = db_connect()){
  querystring = paste0("SELECT * FROM historical_mv")
  rs = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(rs)
  DBI::dbDisconnect(db_con)
  return(dat)
}

