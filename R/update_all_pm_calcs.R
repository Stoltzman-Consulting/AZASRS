#' Update all private market calculations
#' @description This creates csv files for all metrics surroundin
#' @export
update_all_private_market_calculations = function(parallel = FALSE){
  print("Calculating ALL metrics for ALL value dates, it will take some time.")
  print("If possible, use parallel = TRUE in order to DRASTICALLY speed up computation time.")

  # Allow for parallel computation
  if(parallel) future::plan(future::multisession)

  DATA = LOAD_GLOBAL_DATA_PRIVATE_MARKETS()

  con = DATA$con
  value_date = DATA$pm_value_date
  end_date = DATA$pm_value_date
  next_qtr = calc_add_qtrs(value_date, 1)
  nav_daily = DATA$pm_nav_daily
  cf_daily = DATA$pm_cash_flow_daily
  bench_daily = DATA$pm_benchmark_index
  bench_relationships = DATA$pm_benchmark_relationships %>% dplyr::filter(benchmark_type == 'PVT')
  pm_fund_info = DATA$pm_fund_info


  build_all_metrics = function(...){
    portfolio_ = build_grouped_pm_cash_flow(pm_fund_portfolio,...)
    category_ = build_grouped_pm_cash_flow(pm_fund_portfolio, pm_fund_category_description,...)
    sponsor_ = build_grouped_pm_cash_flow(pm_fund_portfolio, pm_fund_sponsor,...)
    fund_ = build_grouped_pm_cash_flow(pm_fund_portfolio, pm_fund_category_description, pm_fund_sponsor, pm_fund_id, pm_fund_description, pm_fund_common_name, ...)
    portfolio = portfolio_ %>% calc_grouped_pm_metrics(pm_fund_portfolio)
    category = category_ %>% calc_grouped_pm_metrics(pm_fund_portfolio, pm_fund_category_description)
    sponsor = sponsor_ %>% calc_grouped_pm_metrics(pm_fund_portfolio, pm_fund_sponsor)
    fund = fund_ %>% calc_grouped_pm_metrics(pm_fund_portfolio, pm_fund_category_description, pm_fund_sponsor, pm_fund_id, pm_fund_description, pm_fund_common_name)
    underlying_data = fund_ %>% dplyr::bind_rows(category_) %>% dplyr::bind_rows(sponsor_) %>% dplyr::bind_rows(portfolio_)
    metrics = fund %>% dplyr::bind_rows(category) %>% dplyr::bind_rows(sponsor) %>% dplyr::bind_rows(portfolio)
    ALL_DATA = underlying_data %>%
      dplyr::mutate(DATA_TYPE = "UNDERLYING") %>%
      dplyr::bind_rows(metrics %>% dplyr::mutate(DATA_TYPE = "METRICS"))
    return(ALL_DATA)
  }
  build_all_pm_data = function(end_date){
    qtrs_back = -1 * 4 * c(0.25, 1, 3, 5, 10)
    start_dates = purrr::map2(end_date, qtrs_back, .f = calc_add_qtrs)
    itd = FALSE
    cash_adjusted = FALSE
    cash_adjusted_all = FALSE
    data = purrr::pmap(
      list(con = list(con),
           start_date = start_dates,
           end_date = list(end_date),
           itd = list(itd),
           cash_adjusted = list(cash_adjusted),
           nav_daily = list(nav_daily),
           cf_daily = list(cf_daily),
           bench_daily = list(bench_daily),
           bench_relationships = list(bench_relationships),
           pm_fund_info = list(pm_fund_info),
           cash_adjusted_all = list(cash_adjusted_all)),
      build_all_metrics
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(cash_adjusted = cash_adjusted,
             itd = itd,
             n_qtrs = round(as.numeric(end_date - start_date)/91.25)) %>%
      calc_time_delta()
    itd = TRUE
    cash_adjusted = FALSE
    cash_adjusted_all = FALSE
    data_itd = purrr::pmap(
      list(con = list(con),
           start_date = list("2001-12-31"),
           end_date = list(end_date),
           itd = list(itd),
           cash_adjusted = list(cash_adjusted),
           nav_daily = list(nav_daily),
           cf_daily = list(cf_daily),
           bench_daily = list(bench_daily),
           bench_relationships = list(bench_relationships),
           pm_fund_info = list(pm_fund_info),
           cash_adjusted_all = list(cash_adjusted_all)),
      build_all_metrics
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(cash_adjusted = cash_adjusted,
             itd = itd,
             n_qtrs = round(as.numeric(end_date - start_date)/91.25)) %>%
      calc_time_delta()
    # Only need cash adjusted if these line up
    if(as.character(end_date) == as.character(next_qtr)){
      itd = FALSE
      cash_adjusted = TRUE
      cash_adjusted_all = FALSE
      data_cash_adj = purrr::pmap(
        list(con = list(con),
             start_date = start_dates,
             end_date = list(end_date),
             itd = list(itd),
             cash_adjusted = list(cash_adjusted),
             nav_daily = list(nav_daily),
             cf_daily = list(cf_daily),
             bench_daily = list(bench_daily),
             bench_relationships = list(bench_relationships),
             pm_fund_info = list(pm_fund_info),
             cash_adjusted_all = list(cash_adjusted_all)),
        build_all_metrics
      ) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(cash_adjusted = cash_adjusted,
               itd = itd,
               n_qtrs = round(as.numeric(end_date - start_date)/91.25)) %>%
        calc_time_delta()
      itd = TRUE
      cash_adjusted = TRUE
      cash_adjusted_all = FALSE
      data_itd_cash_adj = purrr::pmap(
        list(con = list(con),
             start_date = list("2001-12-31"),
             end_date = list(end_date),
             itd = list(itd),
             cash_adjusted = list(cash_adjusted),
             nav_daily = list(nav_daily),
             cf_daily = list(cf_daily),
             bench_daily = list(bench_daily),
             bench_relationships = list(bench_relationships),
             pm_fund_info = list(pm_fund_info),
             cash_adjusted_all = list(cash_adjusted_all)),
        build_all_metrics
      ) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(cash_adjusted = cash_adjusted,
               itd = itd,
               n_qtrs = round(as.numeric(end_date - start_date)/91.25)) %>%
        calc_time_delta()
      ret = dplyr::bind_rows(data, data_itd, data_cash_adj, data_itd_cash_adj) %>%
        dplyr::mutate(period = as.character(period),
               period = dplyr::if_else(
                 itd == TRUE,
                 'ITD',
                 period))
      return(ret)
    }
    return(dplyr::bind_rows(data, data_itd))
  }
  # all_end_dates = c("2020-06-30", "2020-03-31")
  all_end_dates = nav_daily %>% dplyr::filter(effective_date != min(effective_date)) %>% dplyr::select(effective_date) %>% unique() %>% dplyr::pull()
  current_time = Sys.time()

  if(parallel){
    all_data = furrr::future_map(
      all_end_dates,
      build_all_pm_data,
      .options = furrr::future_options(seed = TRUE)
    )
  } else{
    all_data = purrr::map(
      all_end_dates,
      build_all_pm_data
    )
  }


  all_data_df = dplyr::bind_rows(all_data)
  metrics = all_data_df %>% dplyr::filter(DATA_TYPE == 'METRICS') %>% dplyr::select(-DATA_TYPE) %>% dplyr::select_if(~sum(!is.na(.)) > 0)
  metrics %>% readr::write_csv('all_pm_calcs.csv')

  underlying_data = all_data_df %>% dplyr::filter(DATA_TYPE == 'UNDERLYING') %>% dplyr::select(-DATA_TYPE) %>% dplyr::select_if(~sum(!is.na(.)) > 0)
  underlying_data %>% readr::write_csv('all_underlying_cash_flow.csv')

  finished_time = Sys.time()
  print("time:")
  print(finished_time - current_time)

  return(list(metrics = metrics, underlying_data = underlying_data))

}



