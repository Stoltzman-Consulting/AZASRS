#' @export
build_lagged_pm_metrics = function(...,
                   con = AZASRS_DATABASE_CONNECTION(),
                   start_date = '2019-06-30',
                   end_date = get_value_date(con = con),
                   time_delta = 'quarters',
                   n_qtrs = 4,
                   itd = FALSE,
                   return_calcs = TRUE){

  # benchmark_lookup is a tibble to match for DVA & PME calcs
  # benchmark_lookup = tibble::tibble(pm_fund_portfolio = c("Credit", "PE",   "RE"), benchmark_id = c("ODCE",   "ODCE", "LSTA+250"))
  test_exists = dplyr::enquos(...)
  if(is.null(test_exists$benchmark_lookup)){
    benchmark_lookup = default_benchmark_lookup
  }

  min_nav_date = tbl_pm_fund_nav_daily(con) %>%
    dplyr::select(effective_date) %>%
    dplyr::summarize(effective_date = min(effective_date, na.rm = TRUE)) %>%
    dplyr::pull()

  bench_tbl = build_benchmark_fv_index_factor(...,
                                              con = con,
                                              start_date = min_nav_date,
                                              value_date = end_date,
                                              return_tibble = FALSE) %>%
    dplyr::select(-benchmark_info_id, -index_factor) %>%
    tibble::as_tibble()

  bench = benchmark_lookup %>%
    dplyr::left_join(bench_tbl)

  if(itd){
    dates_df = tibble::tibble(start_date = lubridate::as_date('1900-12-31'), end_date = lubridate::as_date(end_date), itd = TRUE)
  } else{
    dates_df = build_lagged_date_range_df(con = con, start_date = start_date, end_date = end_date,
                                          time_delta = time_delta, n_qtrs = n_qtrs) %>%
      dplyr::mutate(itd = FALSE)
  }

  dat_prep = dates_df %>%
    dplyr::group_by(start_date, end_date) %>%
    dplyr::mutate(nav_cash_flow = list(build_nav_cash_flow_combined(...,
                                                                    con = con,
                                                                    start_date = start_date,
                                                                    end_date = end_date,
                                                                    itd = itd,
                                                                    return_tibble = TRUE))) %>%
    tidyr::unnest(cols = c(nav_cash_flow)) %>%
    dplyr::group_by(..., start_date, end_date, effective_date, itd) %>%
    dplyr::summarize(nav_cash_flow = sum(nav_cf, na.rm = TRUE),
                     contributions = sum(contributions, na.rm = TRUE),
                     distributions = sum(distributions, na.rm = TRUE),
                     nav = sum(nav)) %>%
    dplyr::ungroup()

  dat_with_bench = dat_prep %>%
    dplyr::left_join(bench)

  dat_with_bench_end = dat_with_bench %>%
    dplyr::group_by(...) %>%
    dplyr::filter(effective_date == max(effective_date, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(benchmark_id) %>%
    dplyr::summarize(last_index_value = max(index_value, na.rm = TRUE))

  dat_with_bench_factor = dat_with_bench %>%
    dplyr::left_join(dat_with_bench_end, by = 'benchmark_id') %>%
    dplyr::mutate(index_factor = last_index_value / index_value)

  dat = dat_with_bench_factor %>%
    dplyr::group_by(..., start_date, end_date, itd) %>%
    dplyr::arrange(effective_date) %>%
    dplyr::summarize(irr = calc_irr(cash_flow = nav_cash_flow, dates = effective_date),
                     tvpi = calc_tvpi(distributions, contributions, nav),
                     dpi = calc_dpi(distributions, contributions),
                     appreciation = calc_appreciation(contributions+distributions, nav),
                     dva = calc_dva(contributions+distributions, index_factor),
                     pme = calc_pme(distributions, contributions, nav, index_factor)) %>%
    dplyr::mutate(lagged_period = round(as.integer(end_date - start_date)/365, 2),
                  lagged_period = as.character(dplyr::if_else(lagged_period >= 1, round(lagged_period), lagged_period)),
                  lagged_period = dplyr::if_else(lagged_period == '0.25','3 Months',
                                                 dplyr::if_else(lagged_period == '0.50' | lagged_period == '0.5', '6 Months',
                                                                paste(lagged_period, 'Year'))),
                  lagged_period = dplyr::if_else(itd == TRUE, 'ITD', lagged_period)) %>%
    dplyr::ungroup()


  return(dat)
}
