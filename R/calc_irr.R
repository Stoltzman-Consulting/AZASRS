#' @export
calc_irr = function(cash_flow, dates){
  dat = asrsMethods::irr.z(zoo::zoo(cash_flow, dates), gips = TRUE)
  return(dat)
}


#' @export
calc_irr_df_pm = function(group_var = pm_fund_id, date_cutoff='valdate'){
  # Obtain data
  if(date_cutoff == 'valdate'){
    date_cutoff = value_date()
  }else if(date_cutoff == 'next_qtr'){
      date_cutoff = next_quarter()
  }else{
      stop('') #add an important error to state must be valdate or next_qtr
    }

  group_var = enquo(group_var)

  nav_daily_raw = get_pm_nav_daily() %>%
    mutate(effective_date = as.Date(effective_date, format = '%Y-%m-%d')) %>%
    filter(effective_date > '1900-01-01')
  cf_daily_raw = get_pm_cash_flow_daily() %>%
    mutate(effective_date = as.Date(effective_date, format = '%Y-%m-%d')) %>%
    filter(effective_date > '1900-01-01')

  nav_daily = nav_daily_raw %>%
    mutate(nav_cutoff = if_else(effective_date == valdate, nav, 0)) %>%
    select(!! group_var, effective_date, nav_cutoff) %>%
    filter(nav_cutoff != 0) %>%
    mutate(cash_flow = 0)

  nav_cf_daily = cf_daily_raw %>%
    select(!! group_var, effective_date, cash_flow) %>%
    mutate(nav_cutoff = 0) %>%
    bind_rows(nav_daily) %>%
    group_by(!! group_var, effective_date) %>%
    summarize(cash_flow = sum(cash_flow),
              nav_cutoff = sum(nav_cutoff)) %>%
    mutate(cash_flow_cutoff = cash_flow + nav_cutoff)

  # Calculate IRR
  fund_irr = nav_cf_daily %>%
    group_by(!! group_var) %>%
    summarize(irr = calc_irr(cash_flow_cutoff, effective_date))
  return(fund_irr)
}
