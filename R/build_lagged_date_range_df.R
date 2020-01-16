#' @export
build_lagged_date_range_df = function(con = AZASRS_DATABASE_CONNECTION(), start_date = '2017-12-31', end_date = get_value_date(con = con), time_delta = 'quarters', n_qtrs = 4){
  dat = filled_list_of_dates(start_date = start_date, end_date = end_date, time_delta = time_delta) %>%
    dplyr::mutate(start_date = dplyr::lag(effective_date, n_qtrs)) %>% # number of quarters, 4 = 1yr
    dplyr::rename(end_date = effective_date) %>%
    tidyr::drop_na(start_date) %>%
    dplyr::select(start_date, end_date)
  return(dat)
}

