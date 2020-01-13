#' BUILDS IRR's
#'
#' @description Calculates cash flow list properly and adds NAV where required
#' @param ... is the grouping set of variable(s) requested
#' @param start_date is the beginning date you would like the IRR to be calculated from
#' Should be in string format: 'yyyy-mm-dd'
#' @param end_date is the last date you would like the IRR to be calculated to
#' Should be in string format: 'yyyy-mm-dd'
#' @export
calc_grouped_irrs = function(...,
                             con = AZASRS_DATABASE_CONNECTION(),
                              nav_daily = get_pm_nav_daily(con = con, return_tibble = FALSE),
                              cash_flow_daily = get_pm_cash_flow_daily(con = con, return_tibble = FALSE),
                              start_date = '2017-12-31',
                              end_date = get_value_date(con = con)){

  exprs = dplyr::enquos(...)
  rlang::qq_show(dplyr::group_by(!!!exprs))
  nav_min_max_prep = nav_daily %>%
    dplyr::filter(effective_date == start_date | effective_date == end_date)

  nav_min_max = nav_min_max_prep %>%
    dplyr::select(!!!exprs, effective_date, nav)%>%
    dplyr::group_by(!!!exprs, effective_date) %>%
    dplyr::summarize(nav = sum(nav, na.rm = TRUE)) %>%
    dplyr::group_by(!!!exprs) %>%
    dplyr::mutate(nav = if_else(effective_date == start_date, -1*nav, nav)) %>%
    dplyr::rename(nav_cf = nav)

  cash_flows_between = cash_flow_daily %>%
    dplyr::filter(effective_date > start_date, effective_date < end_date) %>%
    dplyr::select(!!!exprs, effective_date, cash_flow) %>%
    dplyr::group_by(!!!exprs, effective_date) %>%
    dplyr::summarize(cash_flow = sum(cash_flow, na.rm = TRUE)) %>%
    dplyr::rename(nav_cf = cash_flow)

  final_dat = dplyr::union(nav_min_max, cash_flows_between) %>%
    dplyr::arrange(!!!exprs, effective_date) %>%
    tibble::as_tibble()

  dat = final_dat %>%
    dplyr::group_by(!!!exprs) %>%
    dplyr::summarize(irr = calc_irr(cash_flow = nav_cf, dates = effective_date))

  return(dat)

}
