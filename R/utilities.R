
#' Calculate previous year and quarter
#'
#' @description Subracts years and quarters to simplify calculations
#' @param start_date is a string (format yyyy-dd-mm)
#' @param years is the number of years to subract (integer)
#' @param qtrs is the number of quarters to subract (integer)
#' @export
calc_previous_year_qtr = function(start_date = get_value_date(), years = 0, qtrs = 0){
  start_date = lubridate::as_date(start_date)
  previous_year = start_date - lubridate::years(years)
  previous_year_qtr = previous_year %m-% months(3*qtrs)
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

# Creates a csv from every single table in db
# library(tidyverse)
# library(AZASRS)
# con = AZASRS_DATABASE_CONNECTION()
# tbl_names = dplyr::src_tbls(con)
#
# for(tbl_name in tbl_names){
#   print(tbl_name)
#   if(tbl_name %in% c('all_account_info', 'all_pm_fund_info', 'all_benchmark_daily')){next}
#   dat = dplyr::tbl(con, tbl_name) %>% as_tibble()
#   write_csv(dat, paste0('data_tables/',tbl_name,'.csv'))
# }
