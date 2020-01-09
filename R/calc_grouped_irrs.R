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
                              end_date = get_value_date(con = con),
                              itd = FALSE){

  grouping_vars = enquos(...)

  if(itd){

    cash_flow_min_dates = cash_flow_daily %>%
      dplyr:: select(grouping_vars, effective_date) %>%
      dplyr::group_by(grouping_vars) %>%
      dplyr::summarize(min_date = min(effective_date, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(!!!grouping_vars, min_date)

    nav_max_dates = nav_daily %>%
      dplyr::select(!!!grouping_vars, effective_date, nav) %>%
      dplyr::group_by(!!!grouping_vars) %>%
      dplyr::filter(effective_date == end_date) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!!grouping_vars, effective_date) %>%
      dplyr::summarize(cash_flow = sum(nav, na.rm = TRUE)) %>%
      dplyr::select(!!!grouping_vars, effective_date, cash_flow)

    cash_flows_between = cash_flow_daily %>%
      dplyr::left_join(cash_flow_min_dates) %>%
      dplyr::filter(effective_date >= min_date & effective_date < end_date) %>%
      dplyr::select(!!!grouping_vars, effective_date, cash_flow) %>%
      dplyr::union_all(nav_max_dates) %>%
      dplyr::group_by(!!!grouping_vars, effective_date) %>%
      dplyr::summarize(cash_flows = sum(cash_flow, na.rm = TRUE)) %>%
      dplyr::arrange(!!!grouping_vars, effective_date) %>%
      tibble::as_tibble()

    dat = cash_flows_between %>%
      dplyr::group_by(!!!grouping_vars) %>%
      dplyr::summarize(irr = calc_irr(cash_flow = cash_flows, dates = effective_date))

    return(dat)
  }


  nav_min_max_prep = nav_daily %>%
    dplyr::filter(effective_date == start_date | effective_date == end_date)

  nav_min_max = nav_min_max_prep %>%
    dplyr::select(!!!grouping_vars, effective_date, nav)%>%
    dplyr::group_by(!!!grouping_vars, effective_date) %>%
    dplyr::summarize(nav = sum(nav, na.rm = TRUE)) %>%
    dplyr::group_by(!!!grouping_vars) %>%
    dplyr::mutate(nav = if_else(effective_date == start_date, -1*nav, nav)) %>%
    dplyr::rename(nav_cf = nav)

  cash_flows_between = cash_flow_daily %>%
    dplyr::filter(effective_date > start_date, effective_date < end_date) %>%
    dplyr::select(!!!grouping_vars, effective_date, cash_flow) %>%
    dplyr::group_by(!!!grouping_vars, effective_date) %>%
    dplyr::summarize(cash_flow = sum(cash_flow, na.rm = TRUE)) %>%
    dplyr::rename(nav_cf = cash_flow)

  final_dat = dplyr::union(nav_min_max, cash_flows_between) %>%
    dplyr::arrange(!!!grouping_vars, effective_date) %>%
    tibble::as_tibble()

  dat = final_dat %>%
    dplyr::group_by(!!!grouping_vars) %>%
    dplyr::summarize(irr = calc_irr(cash_flow = nav_cf, dates = effective_date))

  return(dat)

}
