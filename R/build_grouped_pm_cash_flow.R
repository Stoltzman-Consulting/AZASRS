#' Build PM "Cash Flow" and utilizing NAV to simulate where necessary
#'
#' @description Combine cash flow and NAV data to create a "cash flow" for private market calculations
#' @param .data is from clean_nav_cf()
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
#' @param start_date is the start date of analysis
#' @param end_date is the cutoff date of analysis, typically a value date
#' @param itd is a boolean that determines whether itd (incpetion to date) is included, overrides start date
#' @param cash_adjusted is a boolean that determines if cash is adjusted NAV should be used
#' @param nav_daily is the object of get_pm_nav_daily()
#' @param cf_daily is the object of get_pm_cash_flow_daily()
#' @param bench_daily is the object of get_benchmark_daily_index()
#' @param bench_relationships is the object of get_benchmark_fund_relationship()
#' @param pm_fund_info is the object of get_pm_fund_info()
#' @param cash_adjusted_all overrides cash_adjusted and combines cash_adjusted + reported
#' @export
build_grouped_pm_cash_flow <- function(...,
                                       con = AZASRS_DATABASE_CONNECTION(),
                                       start_date = "2019-06-30",
                                       end_date = get_value_date(con = con),
                                       itd = FALSE,
                                       cash_adjusted = FALSE,
                                       nav_daily = get_pm_nav_daily(con = con),
                                       cf_daily = get_pm_cash_flow_daily(con = con),
                                       bench_daily = get_benchmark_daily_index(con = con, benchmark_type = "PVT", return_tibble = TRUE),
                                       bench_relationships = get_benchmark_fund_relationship(con = con, bench_type = "PVT", return_tibble = TRUE),
                                       pm_fund_info = get_pm_fund_info(con = con),
                                       cash_adjusted_all = FALSE) {


  # Immediately filter out 0 to ensure "not reported" works as intended
  nav_daily <- nav_daily %>% dplyr::filter(nav != 0)
  cf_daily <- cf_daily %>% dplyr::filter(cash_flow != 0)

  # Test to see whether or not data needs to be rolled up
  IS_NOT_AGGREGATED <- test_is_not_rollup(...)

  # ITD failsafe - ensure start_date is before earliest possible pm_fund_cash_flow
  if (itd) {
    start_date <- "2004-06-30"
  }

  #######
  # Determine active funds and those that have / have not reported
  funds_active <- tibble::tibble(pm_fund_id = unique(c(
    nav_daily %>%
      dplyr::filter(effective_date >= calc_add_qtrs(end_date, -1)) %>%
      dplyr::select(pm_fund_id) %>%
      dplyr::pull(),
    cf_daily %>%
      dplyr::filter(effective_date > calc_add_qtrs(end_date, -1)) %>%
      dplyr::select(pm_fund_id) %>%
      dplyr::pull())))

  funds_reported <- tibble::tibble(pm_fund_id = unique(nav_daily %>%
                                                         dplyr::filter(effective_date == end_date) %>%
                                                         dplyr::select(pm_fund_id) %>%
                                                         dplyr::pull()))

  funds_not_reported = funds_active %>%
    dplyr::anti_join(funds_reported, by = 'pm_fund_id')
  #######

  # Cash adjusted should simply create a NAV at the end_date that is previous NAV + cash flows
  if(cash_adjusted){

    if(cash_adjusted_all){
      nav_daily_ = nav_daily %>%
        dplyr::inner_join(funds_active, by = 'pm_fund_id')
      cf_daily_ = cf_daily %>%
        dplyr::inner_join(funds_active, by = 'pm_fund_id')
    } else{
      nav_daily_ = nav_daily %>%
        dplyr::inner_join(funds_not_reported, by = 'pm_fund_id')
      cf_daily_ = cf_daily %>%
        dplyr::inner_join(funds_not_reported, by = 'pm_fund_id')
    }

    first_nav = nav_daily_ %>%
      dplyr::group_by(pm_fund_id) %>%
      dplyr::filter(effective_date == calc_add_qtrs(end_date, -1)) %>%
      dplyr::summarize(nav = dplyr::last(nav, order_by = effective_date)) %>%
      dplyr::ungroup()

    cf_addition_to_nav = cf_daily_ %>%
      dplyr::filter(effective_date > calc_add_qtrs(end_date, -1)) %>%
      dplyr::group_by(pm_fund_id) %>%
      dplyr::summarize(nav = sum(-1 * cash_flow)) %>% #negative allows it to count toward NAV
      dplyr::ungroup()

    last_nav = first_nav %>%
      dplyr::bind_rows(cf_addition_to_nav) %>%
      dplyr::group_by(pm_fund_id) %>%
      dplyr::summarize(nav = sum(nav)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(effective_date = lubridate::as_date(end_date)) %>%
      dplyr::left_join(pm_fund_info, by = 'pm_fund_id')

    nav_daily_ = nav_daily_ %>%
      dplyr::bind_rows(last_nav)

    nav_daily = nav_daily %>%
      #dplyr::anti_join(funds_not_reported, by = 'pm_fund_id') %>%
      dplyr::bind_rows(nav_daily_)

  }


  bench_daily = bench_daily %>%
    dplyr::filter(effective_date >= start_date, effective_date <= end_date) %>%
    dplyr::distinct(benchmark_info_id, effective_date, .keep_all = TRUE) %>%
    dplyr::group_by(benchmark_info_id) %>%
    dplyr::arrange(effective_date) %>%
    dplyr::mutate(index_fv = dplyr::last(index_value) / index_value) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(benchmark_info_id, effective_date)

  cf_prep <- cf_daily %>%
    dplyr::filter(
      effective_date >= start_date,
      effective_date <= end_date
    ) %>%
    dplyr::mutate(nav = 0)

  nav_prep <- nav_daily %>%
    dplyr::filter(effective_date == start_date | effective_date == end_date) %>%
    dplyr::mutate(cash_flow = 0)

  # Combine NAV and CF
  nav_cf <- dplyr::bind_rows(nav_prep, cf_prep)

  # Filter out funds that are not active for the full start - end period.
  # Not applicable to aggregated funds, would filter out important data
  if (IS_NOT_AGGREGATED) {
    if (!itd) {
      nav_cf <- nav_cf %>%
        dplyr::group_by(pm_fund_info_id) %>%
        dplyr::filter(min(effective_date) <= start_date) %>%
        dplyr::filter(max(effective_date) >= end_date) %>%
        dplyr::ungroup()
    }
  }


  # Aggregate by fund & date, convert first NAV values to negative
  nav_cf_prep <- nav_cf %>%
    dplyr::group_by(pm_fund_info_id) %>%
    dplyr::mutate(
      adjusted_cash_flow = 0,
      adjusted_cash_flow = dplyr::if_else(effective_date == start_date, -1 * nav, nav),
      adjusted_cash_flow = adjusted_cash_flow + cash_flow
    ) %>%
    dplyr::group_by(pm_fund_info_id, effective_date) %>%
    dplyr::summarize(
      adjusted_cash_flow = sum(adjusted_cash_flow),
      nav = sum(nav),
      cash_flow = sum(cash_flow)
    ) %>%
    dplyr::ungroup()


  # Join benchmark info and adjust calculations before grouping
  joined_data <- nav_cf_prep %>%
    dplyr::left_join(bench_relationships, by = "pm_fund_info_id") %>%
    dplyr::left_join(bench_daily, by = c("benchmark_info_id", "effective_date")) %>%
    dplyr::left_join(pm_fund_info, by = 'pm_fund_info_id')

  calculated_data <- joined_data %>%
    dplyr::mutate(
      contributions = dplyr::if_else(cash_flow < 0, cash_flow, 0),
      distributions = dplyr::if_else(cash_flow > 0, cash_flow, 0),
      adj_cf_fv = adjusted_cash_flow * index_fv,
      contributions_fv = dplyr::if_else(adj_cf_fv < 0, adj_cf_fv, 0),
      distributions_fv = dplyr::if_else(adj_cf_fv > 0, adj_cf_fv, 0)
    ) %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(
      adj_cf_fv = sum(adj_cf_fv),
      dva = sum(adjusted_cash_flow * index_fv),
      contributions_fv = sum(contributions_fv),
      distributions_fv = sum(distributions_fv),
      contributions = sum(contributions),
      distributions = sum(distributions),
      adjusted_cash_flow = sum(adjusted_cash_flow),
      nav = sum(nav),
      cash_flow = sum(cash_flow)
    ) %>%
    dplyr::ungroup()
}

