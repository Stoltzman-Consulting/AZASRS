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
  valdate = dat[!is.na(dat)][1]
  valdate = as.Date(valdate, format = "%m/%d/%Y")
  valdate = as.Date(valdate)
  DBI::dbClearResult(res)
  DBI::dbDisconnect(db_con)
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
  # Check parameter types
  stopifnot(class(shortname) == 'character' & length(shortname) == 1)

  db_con = DBI::dbConnect(drv = AZASRS_DATABASE_DRIVER, dbname = AZASRS_DATABASE_LOCATION)
  querystring = paste0("SELECT date, shortname, amount FROM nav WHERE shortname = '", shortname, "'")
  res = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(res) %>% tibble::as_tibble()
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
  # Check parameter types
  stopifnot(class(shortname) == 'character' & length(shortname) == 1)

  db_con = DBI::dbConnect(drv = AZASRS_DATABASE_DRIVER, dbname = AZASRS_DATABASE_LOCATION)
  querystring = paste0("SELECT date, shortname, amount FROM cashflow WHERE shortname = '", shortname, "'")
  res = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(res) %>% tibble::as_tibble()
  df = dat %>% tidyr::drop_na()
  DBI::dbClearResult(res)
  DBI::dbDisconnect(db_con)
  return(df)
}


#' Query filtered benchmark data
#'
#' SELECT statement to retrieve benchmark data filtered by shortname
#' If 'Fixed 8' is desired, it can be entered as a shortname but it does not come from the database
#' Fixed 8 is calculated off of the length of ODCE returned from the database
#'
#' @param db_con database connection
#' @param convert_365 boolean to use na.approx to interpolate values with days365 function
#' @param return_zoo boolean to return a zoo object instead of a tibble
#' @return dataframe of shortname, longname, date, price (adds in log price of ODCE as Fixed8)
#' @export
get_filtered_benchmark = function(shortname, convert_365 = FALSE, return_zoo = FALSE){
  # Check parameter types
  stopifnot(class(shortname) == 'character' & length(shortname) == 1)

  if(gsub(' ', '', tolower(shortname)) == 'fixed8'){
    fixed8 = TRUE
    shortname = 'ODCE'
  } else{
    fixed8 = FALSE
  }

  db_con = DBI::dbConnect(drv = AZASRS_DATABASE_DRIVER, dbname = AZASRS_DATABASE_LOCATION)
  querystring = paste0("SELECT date, shortname, longname, price FROM benchmark WHERE shortname = '", shortname , "'")
  res = DBI::dbSendQuery(db_con, querystring)
  dat = DBI::dbFetch(res) %>% tibble::as_tibble()

  ### INSERT functions to add log, exp, days365 etc.

  if(fixed8 == TRUE){
    dat = dat %>%
      dplyr::mutate(shortname = 'fixed8',
                    longname = 'Fixed 8')
    dat$price = exp(cumsum(rep(log(1.08)/365, nrow(dat))))
  }

  if(return_zoo == TRUE){ ### relies on price and date columns (names important)
    dat = zoo::zoo(dat$price, as.Date(dat$date))
  }

  if(convert_365 == TRUE){ ### relies on price and date columns (names important)
    dat_z = zoo::zoo(dat$price, as.Date(dat$date))
    dat_z_365 = interpolateDays365(dat_z)
    dat = dat_z_365
  }

  # Build in days365

  DBI::dbClearResult(res)
  DBI::dbDisconnect(db_con)
  return(dat)
}
