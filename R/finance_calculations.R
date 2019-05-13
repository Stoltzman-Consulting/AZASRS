
#' @export
calc_tvpi_df = function(cash_flow){
  dat = cash_flow %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::summarize(tvpi = asrsMethods::tvpi(cash_flow))
  return(dat)
}


#' @export
calc_irr_df = function(cash_flow){
  dat = split(cash_flow, factor(cash_flow$pm_fund_id)) %>%
    purrr::map(.f = ~asrsMethods::irr.z(zoo::zoo(.$cash_flow, .$effective_date), gips = TRUE)) %>%
    tibble::as.tibble() %>%
    tidyr::gather(pm_fund_id, irr)
  # faster but not accurate... can fix?
  # dat = cash_flow %>%
  #   group_by(pm_fund_id) %>%
  #   summarize(irr = asrsMethods::irr.z(cash_flow, gips=TRUE))
  # return(dat)
}


#' @export
calc_lastinvec_df = function(cash_flow){
  dat = cash_flow %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::summarize(lastinvec = asrsMethods::irr.z(cash_flow, gips=TRUE))
  return(dat)
}


#' @export
calc_dpi_df = function(cash_flow){
  dat = cash_flow %>%
    dplyr::mutate(distributions = dplyr::if_else(cash_flow > 0, abs(cash_flow), 0),
           contributions = dplyr::if_else(cash_flow < 0, abs(cash_flow), 0)) %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::summarize(dpi = sum(distributions) / sum(contributions))
  return(dat)
}


#' @export
calc_appreciation_df = function(cash_flow, nav, valdate){
  dat_nav = nav %>%
    dplyr::filter(effective_date == valdate)

  dat_cf = cash_flow %>%
    dplyr::filter(effective_date >= valdate) %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::summarize(cash_flow = sum(cash_flow))

  dat = dat_cf %>%
    dplyr::left_join(dat_nav, by = 'pm_fund_id') %>%
    dplyr::mutate(appreciation = nav - cash_flow)

  return(dat)
}


#' @export
calc_benchmark_daily_index_df = function(benchmark_daily_index){
  latest_benchmark_daily_index = benchmark_daily_index %>%
    dplyr::group_by(benchmark_id) %>%
    dplyr::summarize(effective_date = max(effective_date)) %>%
    dplyr::left_join(benchmark_daily_index, by = c('benchmark_id', 'effective_date')) %>%
    dplyr::rename(latest_daily_index = index_value) %>%
    dplyr::select(benchmark_id, latest_daily_index)

  final_benchmark_daily_index = benchmark_daily_index %>%
    dplyr::left_join(latest_benchmark_daily_index, by = 'benchmark_id') %>%
    dplyr::mutate(final_daily_index = latest_daily_index / index_value)

  return(final_benchmark_daily_index)
}


#' @export
calc_pme_df = function(cash_flow, nav, benchmark_daily_index, valdate, pmfi){
  bench_index = calc_benchmark_daily_index_df(benchmark_daily_index) %>%
    dplyr::mutate(effective_date = as.Date(effective_date, format = '%Y-%m-%d'))

  nav_filtered = nav %>%
    dplyr::mutate(effective_date = as.Date(effective_date, format = '%Y-%m-%d')) %>%
    dplyr::filter(effective_date == valdate) %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::summarize(nav = max(nav))

  cash_flow_mod = cash_flow %>%
    dplyr::mutate(distributions = dplyr::if_else(cash_flow > 0, abs(cash_flow), 0),
           contributions = dplyr::if_else(cash_flow < 0, abs(cash_flow), 0)) %>%
    dplyr::mutate(effective_date = as.Date(effective_date, format = '%Y-%m-%d')) %>%
    dplyr::left_join(pmfi, by = 'pm_fund_id') %>%
    dplyr::left_join(bench_index, by = c('benchmark_id'='benchmark_id', 'effective_date'='effective_date'))

  pme = cash_flow_mod %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::summarize(dist_no_nav = sum(distributions*final_daily_index),
              contrib = sum(contributions*final_daily_index)) %>%
    dplyr::left_join(nav_filtered, by = 'pm_fund_id') %>%
    dplyr::mutate(pme = (dist_no_nav + nav)/contrib)

  return(pme)
}
