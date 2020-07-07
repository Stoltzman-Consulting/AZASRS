#' Create rollup of IRR based off of dates
#'
#' @description Build IRR for any rollup and date range, including ITD and cash adjusted
#' @param .data is from clean_nav_cf()
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
#' @param start_date is the start date of analysis
#' @param end_date is the cutoff date of analysis
#' @param itd is a boolean that determines whether itd (incpetion to date) is included
#' @param cash_adjusted is a boolean that determines if cash is adjusted
#' @param nav_daily is the object of get_pm_nav_daily()
#' @param cf_daily is the object of get_pm_cash_flow_daily()
#' @param bench_daily is the object of get_benchmark_daily_index()
#' @param bench_relationships is the object of get_benchmark_fund_relationship()
#' @param pm_fund_info is the object of get_pm_fund_info()
#' @return
#' @examples
#' # Example use case
#' nav <- get_pm_nav_daily() %>% dplyr::filter(nav != 0)
#' cf <- get_pm_cash_flow_daily() %>% dplyr::filter(cash_flow != 0)
#' pm_fund_info <- get_pm_fund_info()
#' start_date <- "2019-06-30"
#' end_date <- "2019-09-30"
#' itd <- FALSE
#' cash_adjusted <- FALSE
#' final_data <- build_grouped_irrs(
#'   start_date = start_date, end_date = end_date, itd = itd,
#'   cash_adjusted = cash_adjusted, pm_fund_info = pm_fund_info,
#'   pm_fund_portfolio, pm_fund_category_description
#' )
#' @export
build_grouped_pm_metrics <- function(...,
                                     con = AZASRS_DATABASE_CONNECTION(),
                                     start_date = '2016-12-31',
                                     end_date = get_value_date(con = con),
                                     itd = FALSE,
                                     cash_adjusted = FALSE,
                                     nav_daily = get_pm_nav_daily(con = con),
                                     cf_daily = get_pm_cash_flow_daily(con = con),
                                     bench_daily = get_benchmark_daily_index(con = con, benchmark_type = 'PVT', return_tibble = TRUE),
                                     bench_relationships = get_benchmark_fund_relationship(con = con, bench_type = 'PVT', return_tibble = TRUE),
                                     pm_fund_info = get_pm_fund_info(con = con)) {

  clean_data <- build_grouped_pm_cash_flow(...,
                                          start_date = start_date,
                                          end_date = end_date,
                                          itd = itd,
                                          cash_adjusted = cash_adjusted,
                                          nav_daily = nav_daily,
                                          cf_daily = cf_daily,
                                          bench_daily = bench_daily,
                                          bench_relationships = bench_relationships,
                                          pm_fund_info = pm_fund_info
                                          )



  # Append benchmark data before calculating metrics
  clean_data %>%
    calculate_grouped_pm_metrics(...)
}


#' Calculate the rollup IRR
#'
#' @description Calculates the IRR from a list of "cash flow" from clean_nav_cf()
#' @param .data is from clean_nav_cf()
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
calculate_grouped_pm_metrics <- function(.data, ...) {
  .data %>%
    dplyr::group_by(...) %>%
    dplyr::arrange(effective_date) %>%
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
      distributions = sum(distributions)
    )
}
