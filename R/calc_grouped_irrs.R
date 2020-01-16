#' Calculates IRR's given a date range
#'
#' @description Calculates cash flow list properly and adds NAV where required
#' @param ... is the grouping set of variable(s) requested
#' @param start_date is the beginning date you would like the IRR to be calculated from
#' Should be in string format: 'yyyy-mm-dd'
#' @param end_date is the last date you would like the IRR to be calculated to
#' Should be in string format: 'yyyy-mm-dd'
#' @examples
#' con = calc_grouped_irrs(pm_fund_portfolio, pm_fund_category, start_date = '2018-12-31')
#' @export
calc_grouped_irrs = function(...,
                             con = AZASRS_DATABASE_CONNECTION(),
                             nav_daily = get_pm_nav_daily(con = con, return_tibble = FALSE),
                             cash_flow_daily = get_pm_cash_flow_daily(con = con, return_tibble = FALSE),
                             start_date = '2017-12-31',
                             end_date = get_value_date(con = con),
                             itd = FALSE){

  if(itd){start_date = '1899-12-31'} # Allows for ALL data to be unfiltered

  exprs = dplyr::enquos(...)

  nav_min_prep = nav_daily %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::summarize(min_date = min(effective_date, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(min_date = dplyr::if_else(lubridate::as_date(start_date) < min_date, min_date, start_date)) %>%
    dplyr::rename(effective_date = min_date) %>%
    dplyr::mutate(min_nav_date = 1)

  nav_min_max_prep = nav_daily %>%
    dplyr::select(pm_fund_id, effective_date, nav) %>%
    dplyr::left_join(nav_min_prep, by = c('pm_fund_id', 'effective_date')) %>%
    dplyr::filter(effective_date <= end_date) %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::filter(effective_date == end_date | min_nav_date == 1) %>%
    # dplyr::filter(effective_date == max(effective_date, na.rm = TRUE) | min_nav_date == 1) %>% # no need if cash flow accounts for last nav
    dplyr::select(pm_fund_id, effective_date, nav, min_nav_date) %>%
    dplyr::mutate(min_nav_date = dplyr::if_else(is.na(min_nav_date), 0, 1))

  nav_min_max = nav_min_max_prep %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(nav = dplyr::if_else(min_nav_date == 1, -1*nav, nav)) %>%
    dplyr::mutate(cash_flow = 0) %>%
    dplyr::select(-min_nav_date)

  cash_flows_between = cash_flow_daily %>%
    dplyr::filter(effective_date >= start_date, effective_date < end_date) %>%
    dplyr::select(pm_fund_id, effective_date, cash_flow) %>%
    dplyr::group_by(pm_fund_id, effective_date) %>%
    dplyr::summarize(cash_flow = sum(cash_flow, na.rm = TRUE)) %>%
    dplyr::mutate(nav = 0)

  if(itd){
    final_dat_prep = dplyr::union(nav_min_max, cash_flows_between) %>%
      dplyr::left_join(get_pm_fund_info(con = con, return_tibble = FALSE), by = 'pm_fund_id') %>%
      dplyr::group_by(!!!exprs, effective_date) %>%
      dplyr::summarize(nav = sum(nav, na.rm = TRUE),
                       cash_flow = sum(cash_flow, na.rm = TRUE)) %>%
      dplyr::mutate(min_date = min(effective_date, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(nav = dplyr::if_else(effective_date > min_date & effective_date != end_date, 0, nav),
                    cash_flow = dplyr::if_else(effective_date > min_date & effective_date != end_date, cash_flow, 0),
                    nav_cf = nav + cash_flow) %>%
      dplyr::select(!!!exprs, effective_date, nav_cf)

    final_dat = final_dat_prep %>%
      dplyr::group_by(!!!exprs, effective_date) %>%
      dplyr::summarise(nav_cf = sum(nav_cf))
  } else {
    final_dat_prep = dplyr::union(nav_min_max, cash_flows_between) %>%
      dplyr::left_join(get_pm_fund_info(con = con, return_tibble = FALSE), by = 'pm_fund_id') %>%
      dplyr::mutate(nav = dplyr::if_else(effective_date == start_date | effective_date == end_date, nav, 0),
                    cash_flow = dplyr::if_else(effective_date >= start_date & effective_date < end_date, cash_flow, 0)) %>%
      dplyr::group_by(!!!exprs, effective_date) %>%
      dplyr::summarize(nav_cf = sum(nav) + sum(cash_flow)) %>%
      dplyr::select(!!!exprs, effective_date, nav_cf)

    final_dat = final_dat_prep %>%
      dplyr::group_by(!!!exprs, effective_date) %>%
      dplyr::summarise(nav_cf = sum(nav_cf))
  }

  dat = final_dat %>%
    tibble::as_tibble() %>%
    dplyr::group_by(!!!exprs) %>%
    dplyr::summarize(irr = calc_irr(cash_flow = nav_cf, dates = effective_date))

  return(dat)

}
