#' @export
calc_irr = function(cash_flow, dates){
  dat = asrsMethods::irr.z(zoo::zoo(cash_flow, dates), gips = TRUE)
  return(dat)
}


#' @export
calc_irr_df_pm = function(nav_daily = get_pm_nav_daily(),
                          cf_daily = get_pm_cash_flow_daily(),
                          date_cutoff = value_date(), ...){

  group_vars = dplyr::enquos(...)

  nav_daily = nav_daily %>%
    dplyr::mutate(nav_cutoff = if_else(effective_date == valdate, nav, 0)) %>%
    dplyr::select(!!! group_vars, effective_date, nav_cutoff) %>%
    dplyr::filter(nav_cutoff != 0) %>%
    dplyr::mutate(cash_flow = 0)

  nav_cf_daily = cf_daily %>%
    dplyr::select(!!! group_vars, effective_date, cash_flow) %>%
    dplyr::mutate(nav_cutoff = 0) %>%
    dplyr::bind_rows(nav_daily) %>%
    dplyr::group_by(!!! group_vars, effective_date) %>%
    dplyr::summarize(cash_flow = sum(cash_flow),
              nav_cutoff = sum(nav_cutoff)) %>%
    dplyr::mutate(cash_flow_cutoff = cash_flow + nav_cutoff)

  # Calculate IRR
  fund_irr = nav_cf_daily %>%
    dplyr::group_by(!!! group_vars) %>%
    dplyr::summarize(irr = calc_irr(cash_flow_cutoff, effective_date))
  return(fund_irr)
}
