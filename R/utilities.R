
#' Calculate previous year and quarter
#'
#' @description Subracts years and quarters to simplify calculations
#' @param start_date is a string (format yyyy-dd-mm)
#' @param years is the number of years to subract (integer)
#' @param qtrs is the number of quarters to subract (integer)
#' @export
calc_previous_year_qtr = function(end_date = get_value_date(), years = 0, qtrs = 0){
  end_date = lubridate::as_date(end_date)
  previous_year = end_date - lubridate::years(years)
  previous_year_qtr = previous_year %m-% months(3*qtrs)
  previous_year_qtr = lubridate::round_date(previous_year_qtr, unit = 'quarter') - lubridate::days(1)
  return(previous_year_qtr)
}


#' @export
tibble_to_zoo_list = function(tibb, omit_na = TRUE){
  listify = function(x){
    ts_list = list()
    columns = colnames(x)
    for(i in 2:ncol(x)){
      if(omit_na == TRUE){
        ts_list[[columns[i]]] = x[,i] %>% na.omit()
      }
      else{
        ts_list[[columns[i]]] = x[,i]
      }
    }
    return(ts_list)
  }
  dat = tibb %>%
    tsbox::ts_zoo() %>%
    listify()
  return(dat)
}


#' @export
filled_list_of_dates = function(start_date = '1969-12-31', end_date = get_value_date(), time_delta = 'quarters'){
  quarter_ends = c('-03-31', '-06-30', '-09-30', '-12-31') # ensure dates land on proper NAV dates

  if(is.na(!match(substring(start_date, 5), quarter_ends)) | is.na(!match(substring(end_date, 5), quarter_ends))){
    warning(paste0('Your start_date or end_date needs to end in one of the following ', quarter_ends))
    break
  }

  start_date = lubridate::as_date(start_date)
  end_date = lubridate::as_date(end_date) + lubridate::days(1) # add 1 to include end_date in results
  date_seq = seq(start_date, end_date, by = time_delta)
  final_dates = tibble::tibble(date = date_seq) %>%
    dplyr::mutate(date = lubridate::round_date(date, unit = 'quarters') - lubridate::days(1)) %>% #round dates to fix
    dplyr::arrange(date)

  return(final_dates)
}
