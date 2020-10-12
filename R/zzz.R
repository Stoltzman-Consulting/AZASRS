#' Disconnect from database
#' @description Disconnect from database if using AZASRS_DATABASE_CONNECTION
#' @param con object from AZASRS_DATABASE_CONNECTION()
#' @return boolean for DBI disconnected or not
#' @examples
#' con <- AZASRS_DATABASE_CONNECTION()
#' data <- tbl_pm_fund_nav_daily(con) %>%
#'   left_join(tbl_pm_fund_info(con), by = "pm_fund_id") %>%
#'   as_tibble()
#' AZASRS_DATABASE_DISCONNECT(con)
#' @export
GLOBAL_DATA_PRIVATE_MARKETS <- function(pm_con = AZASRS_DATABASE_CONNECTION()) {

  pm_value_date = get_value_date(con = pm_con)
  pm_fund_info = get_pm_fund_info(con = pm_con, add_benchmark = TRUE, return_tibble = TRUE)
  pm_cash_flow_daily = get_pm_cash_flow_daily(con = pm_con, return_tibble = TRUE)
  pm_nav_daily = get_pm_nav_daily(con = pm_con, return_tibble = TRUE)
  pm_benchmark_relationships = get_benchmark_fund_relationship(con = pm_con, get_all_benchmark_types = TRUE, return_tibble = TRUE)
  pm_benchmark_index = get_benchmark_daily_index(con = pm_con, all_bench_types = TRUE, return_tibble = TRUE)

  possible_end_dates = pm_nav_daily %>%
    dplyr::distinct(effective_date) %>%
    dplyr::arrange(effective_date) %>%
    dplyr::pull()

  calculate_start_dates = function(end_date){
    # 50 Years is considered to be ITD
    qtrs_back = -1 * 4 * c(0.25, 0.5, 1, 3, 5, 7, 10, 50)
    start_dates = purrr::map2(end_date, qtrs_back, .f = calc_add_qtrs)
    start_dates_flattened = do.call("c", start_dates)
    start_dates_list = list()
    start_dates_list[[as.character(end_date)]] = start_dates_flattened
    return(start_dates_list)
  }

  pm_date_ranges = do.call("c", purrr::map(possible_end_dates, .f = calculate_start_dates))

  all_data = list(
    pm_con = pm_con,
    pm_fund_info = pm_fund_info,
    pm_cash_flow_daily = pm_cash_flow_daily,
    pm_nav_daily = pm_nav_daily,
    pm_benchmark_relationships = pm_benchmark_relationships,
    pm_benchmark_index = pm_benchmark_index,
    pm_date_ranges = pm_date_ranges
  )

  return(all_data)

}
