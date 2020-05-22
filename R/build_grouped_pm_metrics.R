#' Create rollup of IRR based off of dates
#'
#' @description Build IRR for any rollup and date range, including ITD and cash adjusted
#' @param .data is from clean_nav_cf()
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
#' @examples # Example use case
#' nav = get_pm_nav_daily() %>% dplyr::filter(nav != 0)
#' cf = get_pm_cash_flow_daily() %>% dplyr::filter(cash_flow != 0)
#' pm_fund_info = get_pm_fund_info()
#' start_date = '2019-06-30'
#' end_date = '2019-09-30'
#' itd = FALSE
#' cash_adjusted = FALSE
#' final_data = build_grouped_irrs(start_date = start_date, end_date = end_date, itd = itd,
#'                                 cash_adjusted = cash_adjusted, pm_fund_info = pm_fund_info,
#'                                 pm_fund_portfolio, pm_fund_category_description)
#' @export
build_grouped_pm_metrics = function(con = AZASRS::AZASRS_DATABASE_CONNECTION(),
                                    start_date,
                                    end_date,
                                    itd,
                                    cash_adjusted,
                                    benchmark_type = 'PVT',
                                    nav_daily = get_pm_nav_daily(con = con),
                                    cf_daily = get_pm_cash_flow_daily(con = con),
                                    benchmark_daily_index = get_benchmark_daily_index(con = con,
                                                                                      all_benchmark_types = TRUE,
                                                                                      return_tibble = TRUE),
                                    benchmark_daily_index_fv = build_benchmark_fv_index_factor(con = con,
                                                                                               end_date = end_date,
                                                                                               benchmark_daily_index = benchmark_daily_index),
                                    benchmark_relationships = get_benchmark_fund_relationship(con = con,
                                                                                              benchmark_type = benchmark_type,
                                                                                              all_benchmark_types = FALSE,
                                                                                              return_tibble = TRUE),
                                    pm_fund_info = get_pm_fund_info(con = con),
                                    ...){

  clean_data = build_grouped_pm_cash_flow(con = local(con),
                                          start_date = local(start_date),
                                          end_date = local(end_date),
                                          itd = local(itd),
                                          benchmark_type = local(benchmark_type),
                                          cash_adjusted = local(cash_adjusted),
                                          nav_daily = local(nav_daily),
                                          cf_daily = local(cf_daily),
                                          benchmark_daily_index = local(benchmark_daily_index),
                                          benchmark_daily_index_fv = local(benchmark_daily_index_fv),
                                          benchmark_relationships = local(benchmark_relationships),
                                          pm_fund_info = local(pm_fund_info),
                                          ...)

  # Append benchmark data before calculating metrics
  clean_data %>%
    calculate_grouped_pm_metrics(...)

}


#' Calculate the rollup IRR
#'
#' @description Calculates the IRR from a list of "cash flow" from clean_nav_cf()
#' @param .data is from clean_nav_cf()
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
calculate_grouped_pm_metrics = function(.data, ...){
  .data %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      pme = -sum(distributions_fv) / sum(contributions_fv),
      irr = calc_irr(adjusted_cash_flow, effective_date),
      irr_fv = calc_irr(cash_flow = adj_cf_fv, dates = effective_date),
      tvpi = calc_tvpi(distributions = distributions, contributions = contributions, nav = nav),
      alpha = log(1 + irr_fv),
      bench_irr = -1 + exp(log(1 + irr) - alpha),
      dva = sum(dva),
      nav = dplyr::last(nav),
      # nav = sum(nav),
      cash_flow = sum(cash_flow),
      adjusted_cash_flow = sum(adjusted_cash_flow),
      contributions = sum(contributions),
      distributions = sum(distributions))
}
