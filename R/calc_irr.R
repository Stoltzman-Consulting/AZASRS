#' @export
calc_irr = function(cash_flow, dates){
  dat = asrsMethods::irr.z(zoo::zoo(cash_flow, dates), gips = TRUE)
  return(dat)
}


#' @export
calc_pm_metrics_df = function(nav_daily = get_pm_nav_daily(),
                          cf_daily = get_pm_cash_flow_daily(),
                          benchmark_daily = get_benchmark_daily_index(),
                          date_start = '1900-01-01',
                          date_cutoff = value_date(),
                          ...){

  group_vars = dplyr::enquos(...)

  pmfi = nav_daily %>% dplyr::select(pm_fund_id,
                                     pm_fund_description,
                                     pm_fund_category,
                                     pm_fund_sponsor,
                                     pm_fund_city,
                                     pm_fund_sector,
                                     pm_fund_portfolio) %>% unique()

  #### filtering all dates
  nav_daily = nav_daily %>% dplyr::filter(effective_date >= date_start & effective_date <= date_cutoff)
  cf_daily = cf_daily %>% dplyr::filter(effective_date >= date_start & effective_date <= date_cutoff)
  benchmark_daily = benchmark_daily %>% dplyr::filter(effective_date >= date_start & effective_date <= date_cutoff)



  bench_daily = benchmark_daily %>% dplyr::select(pm_fund_id, effective_date, index_value)

  cf_bench_daily = cf_daily %>%
    dplyr::left_join(bench_daily, by = c('pm_fund_id', 'effective_date')) %>%
    dplyr::select(pm_fund_id, effective_date, cash_flow, contributions, distributions, index_value)

    # nav_cutoff - not cash_adjusted, should we add that? would be additional if_else
  nav_daily = nav_daily %>%
    dplyr::mutate(nav_cutoff = if_else(effective_date == valdate, nav, 0)) %>%
    dplyr::select(pm_fund_id, effective_date, nav_cutoff) %>%
    dplyr::filter(nav_cutoff != 0) %>%
    dplyr::mutate(cash_flow = 0, contributions = 0, distributions = 0)

  nav_cf_daily = cf_bench_daily %>%
    dplyr::mutate(nav_cutoff = 0) %>%
    dplyr::bind_rows(nav_daily) %>%
    dplyr::group_by(pm_fund_id, effective_date) %>%
    dplyr::summarize(cash_flow = sum(cash_flow),
              nav_cutoff = sum(nav_cutoff),
              contributions = sum(contributions),
              distributions = sum(distributions)) %>%
    dplyr::mutate(cash_flow_cutoff = cash_flow + nav_cutoff) %>%
    dplyr::left_join(bench_daily, by = c('pm_fund_id', 'effective_date'))

  nav_cf_daily[is.na(nav_cf_daily)] = 0

  #PME setup calcs
  fv_index_factors = nav_cf_daily %>%
    dplyr::filter(effective_date == date_cutoff) %>%
    unique() %>%
    dplyr::select(pm_fund_id, index_value) %>%
    dplyr::rename(last_index_value = index_value)

  nav_cf_w_fv = nav_cf_daily %>%
    dplyr::left_join(fv_index_factors, by = 'pm_fund_id') %>%
    mutate(fv_index_factors =  last_index_value / index_value)

  # Calculate IRR, DPI, TVPI, Appreciation
  fund_metrics = nav_cf_w_fv %>%
    dplyr::left_join(pmfi, by = 'pm_fund_id') %>%
    dplyr::group_by(!!! group_vars) %>%
    dplyr::summarize(irr = calc_irr(cash_flow_cutoff, effective_date),
                     dpi = calc_dpi(distributions, contributions),
                     tvpi = calc_tvpi(distributions, contributions, nav_cutoff),
                     appreciation = calc_appreciation(nav_cutoff, cash_flow),
                     pme = calc_pme(distributions, contributions, fv_index_factors, nav_cutoff))

  return(fund_metrics)
}
