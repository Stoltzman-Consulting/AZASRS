# Place to test functions... can later decide where to organize these

#' Query cashflow data
#'
#' SELECT statement to retrieve all cashflow data
#'
#' @param db_con database connection
#' @return cashflow dataframe (date, shortname, amount)
#' @export
get_cashflow = function(from_date = '2017-01-01', short_name = '', return_type = 'tibble'){
  db_con = DBI::dbConnect(drv = AZASRS_DATABASE_DRIVER, dbname = AZASRS_DATABASE_LOCATION)

  if(short_name == ''){
    df = dplyr::tbl(db_con, "cashflow") %>%
      dplyr::select(date, shortname, amount) %>%
      dplyr::filter(date >= from_date) %>%
      tibble::as_tibble()
    DBI::dbDisconnect(db_con)

  } else {
    df = dplyr::tbl(db_con, "cashflow") %>%
      dplyr::select(date, shortname, amount) %>%
      dplyr::filter(date >= from_date) %>%
      dplyr::filter(shortname == short_name) %>%
      tibble::as_tibble()
    DBI::dbDisconnect(db_con)

  }

  if(return_type == 'zoo' & short_name != ''){
    df = zoo::zoo(df$amount, df$date)
  }

  return(df)
  }
