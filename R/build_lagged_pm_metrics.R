#' @export
build_lagged_pm_metrics = function(...,
                   con = AZASRS_DATABASE_CONNECTION(),
                   start_date = '2016-12-31',
                   end_date = get_value_date(con = con),
                   time_delta = 'quarters',
                   n_qtrs = 4,
                   return_calcs = TRUE){

  # benchmark_lookup is a tibble to match for DVA & PME calcs
  # benchmark_lookup = tibble::tibble(pm_fund_portfolio = c("Credit", "PE",   "RE"), benchmark_id = c("ODCE",   "ODCE", "LSTA+250"))
  test_existing = dplyr::enquos(...)
  if(is.null(test_existing$benchmark_lookup)){
    benchmark_lookup = default_benchmark_lookup
  }



  bench_tbl = build_benchmark_fv_index_factor(...,
                                              con = con, # con defined in parent function
                                              start_date = start_date,
                                              value_date = end_date,
                                              return_tibble = FALSE) %>%
    dplyr::select(-benchmark_info_id, -index_factor) %>%
    tibble::as_tibble()

  bench = benchmark_lookup %>%
    dplyr::left_join(bench_tbl)

  dates_df = build_lagged_date_range_df(con = con, start_date = start_date, end_date = end_date, time_delta = time_delta, n_qtrs = n_qtrs)

  dat = dates_df %>%
    dplyr::group_by(start_date, end_date) %>%
    dplyr::mutate(nav_cash_flow = list(build_nav_cash_flow_combined(...,
                                                                con = con,
                                                                start_date = start_date,
                                                                end_date = end_date,
                                                                return_tibble = TRUE)))

    tmp_irr_calc = function(df){
      deselect_cols = paste(c('nav_cf', 'distributions', 'contributions', 'nav', 'index_value', 'index_factor'), collapse = "|")
      deselect_cols_lower_level = paste(c('effective_date', 'nav_cf', 'distributions', 'contributions', 'nav', 'index_value', 'index_factor'), collapse = "|")

      dat_prep = df %>%
        dplyr::group_by_at(names(df)[-grep(deselect_cols, names(df))]) %>%
        dplyr::summarize(nav_cf = sum(nav_cf, na.rm = TRUE),
                         distributions = sum(distributions, na.rm = TRUE),
                         contributions = sum(contributions, na.rm = TRUE),
                         nav = sum(nav, na.rm = TRUE))

      dat_with_bench = dat_prep %>%
        dplyr::left_join(bench) # variable from parent function

      dat_with_bench_end = dat_with_bench %>%
        dplyr::group_by_at(names(dat_with_bench)[-grep(deselect_cols_lower_level, names(dat_with_bench))]) %>%
        dplyr::filter(effective_date == max(effective_date, na.rom = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(benchmark_id, index_value) %>%
        dplyr::distinct(benchmark_id, index_value) %>%
        dplyr::rename(last_index_value = index_value)

      dat_with_bench_factor = dat_with_bench %>%
        dplyr::left_join(dat_with_bench_end, by = 'benchmark_id') %>%
        dplyr::mutate(index_factor = last_index_value / index_value)

      dat = dat_with_bench_factor %>%
        dplyr::group_by_at(names(dat_with_bench_factor)[-grep(deselect_cols_lower_level, names(dat_with_bench_factor))]) %>%
        dplyr::summarize(irr = calc_irr(cash_flow = nav_cf, dates = effective_date),
                         tvpi = calc_tvpi(distributions, contributions, nav),
                         dpi = calc_dpi(distributions, contributions),
                         appreciation = calc_appreciation(contributions+distributions, nav),
                         dva = calc_dva(contributions+distributions, index_factor),
                         pme = calc_pme(distributions, contributions, nav, index_factor))

      return(dat)
    }

    dat = dat %>%
      dplyr::group_by(start_date, end_date) %>%
      dplyr::mutate(irr = purrr::pmap(.l = list(nav_cash_flow),
                                      .f = tmp_irr_calc)) %>%
      dplyr::select(-nav_cash_flow) %>%
      tidyr::unnest(cols = c(irr))

  return(dat)
}
