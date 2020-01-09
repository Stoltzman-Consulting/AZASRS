#' Build a tibble of rolling IRRs by any grouping
#'
#' @param ... grouping variables (pm_fund_id, pm_fund_portfolio, etc.)
#' @param start_date is first date
#' @param end_date is last date
#' @param time_delta is type of lag (quarters or years)
#' @param n_qtrs is how far back the lookback window should be (4 = 1 year, 20 = 5 years if time_delta is quarters)
#' @param itd whether or not you would like to have an ITD variable appended
#' @return Returns a tibble with grouping variables and all of their respective metrics
#' @export
build_grouped_irrs = function(...,
                              con = AZASRS::AZASRS_DATABASE_CONNECTION(),
                              start_date = '2017-12-31',
                              end_date = get_value_date(con = con),
                              time_delta = 'quarters',
                              n_qtrs = 4,
                              itd = FALSE){

  grouping_vars = enquos(...)

  my_dates = filled_list_of_dates(start_date = start_date, end_date = end_date, time_delta = time_delta) %>%
    dplyr::mutate(start_date = dplyr::lag(date, n_qtrs), con = list(con)) %>% # number of quarters, 4 = 1yr
    dplyr::rename(end_date = date) %>%
    tidyr::drop_na(start_date) %>%
    dplyr::mutate(itd = FALSE)

  dat = my_dates %>%
    dplyr::mutate(irr = purrr::pmap(.l = list(enexpr(grouping_vars)[1],
                                              con = con,
                                              start_date = start_date,
                                              end_date = end_date,
                                              itd = itd),
                                    calc_grouped_irrs)) %>%
    dplyr::select(start_date, end_date, irr) %>%
    tidyr::unnest(cols = c(irr))

  a=0
  return(dat)

}
