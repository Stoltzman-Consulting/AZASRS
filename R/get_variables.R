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
  valdate = as.Date(valdate, format = "%m/%d/%Y")
  valdate = as.Date(valdate)
  return(valdate)
}


#' Get nav values
#'
#' Queries values with filters
#'
#' @param db_con database connection
#' @param shortname character - shortname is the symbol
#' @return date, shortname, amount
#' @export
get_filtered_nav = function(shortname){
  # Ensure argument
  stopifnot(class(shortname) == 'character' & length(shortname) == 1)

  db_con = DBI::dbConnect(drv = AZASRS_DATABASE_DRIVER, dbname = AZASRS_DATABASE_LOCATION)
  querystring = paste0("SELECT date, shortname, amount FROM nav WHERE shortname = '", shortname, "'")
  res = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(res)
  df = dat %>% tidyr::drop_na()
  DBI::dbClearResult(res)
  DBI::dbDisconnect(db_con)
  return(df)
}


#' Get cashflow
#'
#' Queries cashflow with filters
#'
#' @param db_con database connection
#' @param shortname shortname
#' @return date, shortname, amount
#' @export
get_filtered_cashflow = function(shortname){
  db_con = DBI::dbConnect(drv = AZASRS_DATABASE_DRIVER, dbname = AZASRS_DATABASE_LOCATION)
  querystring = paste0("SELECT date, shortname, amount FROM cashflow WHERE shortname = '", shortname, "'")
  res = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(res)
  df = dat %>% tidyr::drop_na()
  DBI::dbClearResult(res)
  DBI::dbDisconnect(db_con)
  return(df)
}


#' Query filtered benchmark data
#'
#' SELECT statement to retrieve benchmark data filtered by shortname
#'
#' @param db_con database connection
#' @return dataframe of shortname, longname, date, price (adds in log price of ODCE as Fixed8)
#' @export
get_filtered_benchmark = function(shortname = ''){
  db_con = DBI::dbConnect(drv = AZASRS_DATABASE_DRIVER, dbname = AZASRS_DATABASE_LOCATION)
  if(shortname == ''){
    querystring = paste0("SELECT date, shortname, longname, price FROM benchmark")
  } else{
    querystring = paste0("SELECT date, shortname, longname, price FROM benchmark WHERE shortname = '", shortname , "'")
  }
  res = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(res)
  DBI::dbClearResult(res)

  ### INSERT functions to add log, exp, days365 etc?

  DBI::dbDisconnect(db_con)
  return(dat)
}
