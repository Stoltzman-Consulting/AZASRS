#' @export
calc_pm_metrics_df = function(nav_daily = get_pm_nav_daily(),
                          cf_daily = get_pm_cash_flow_daily(),
                          benchmark_daily = get_benchmark_daily_index(),
                          date_start = '1900-01-01',
                          date_cutoff = as.character(lubridate::today()),
                          valdate = value_date(), ...){

  force_first_nav_negative = TRUE

  if(as.Date(date_cutoff) < as.Date(valdate)){stop("date_cutoff must be greater than or equal to valdate")} # ensures potential for pcap after valdate
  if(as.Date(valdate) <= as.Date(date_start)){stop("valdate must be greater than date_start")} # ensures potential for at least one nav or cash_flow before valdate

  # Convert group_vars to character vector for joining tables
  group_vars = dplyr::enquos(...)
  group_vars_char = c()
  for(i in as.character(quos(!!! group_vars))){group_vars_char = c(group_vars_char, substring(as.character(i), 2))}

  #### filtering all dates
  nav_daily_filtered = nav_daily %>% dplyr::filter(effective_date >= date_start & effective_date <= date_cutoff)
  cf_daily_filtered = cf_daily %>% dplyr::filter(effective_date >= date_start & effective_date <= date_cutoff)
  benchmark_daily_filtered = benchmark_daily %>% dplyr::filter(effective_date >= date_start & effective_date <= date_cutoff)

  pmfi = nav_daily_filtered %>% dplyr::select(pm_fund_id,
                                     pm_fund_description,
                                     pm_fund_category,
                                     pm_fund_category_description,
                                     pm_fund_sponsor,
                                     pm_fund_city,
                                     pm_fund_sector,
                                     pm_fund_portfolio) %>% unique()


  # Done creating _filtered data

  bench_daily = benchmark_daily_filtered %>% dplyr::select(pm_fund_id, effective_date, index_value)


  ### benchmark issue!!! this needs each day to count to final right?? this would filter to only same days as join allows
  cf_bench_daily = cf_daily_filtered %>%
    dplyr::left_join(bench_daily, by = c('pm_fund_id', 'effective_date')) %>%
    dplyr::select(pm_fund_id, effective_date, cash_flow, contributions, distributions, index_value)

  # get nav values for: first, valdate (or date cutoff specified), and last in date range
  nav_date_cutoffs = nav_daily_filtered %>%
    dplyr::group_by(pm_fund_id) %>%
      dplyr::mutate(beg_date = min(effective_date), end_date = max(effective_date)) %>%
      dplyr::ungroup() %>%
    dplyr::mutate(beg_nav = dplyr::if_else(effective_date == beg_date, nav, 0),
                  end_nav = dplyr::if_else(effective_date == end_date, nav, 0),
                  val_nav = dplyr::if_else(effective_date == valdate, nav, 0)) %>%
    dplyr::mutate(beg_nav = if(force_first_nav_negative == TRUE){-1*abs(beg_nav)} else{beg_nav}) %>%
    dplyr::mutate(nav_val = beg_nav + val_nav,
                  nav_end = beg_nav + end_nav,
                  pcap = dplyr::if_else(nav_val != nav_end & nav_end != 0, TRUE, FALSE)) %>%
    dplyr::filter(effective_date == beg_date | effective_date == end_date | effective_date == valdate) %>% # TODO: check these assumptions
    dplyr::select(pm_fund_id, effective_date, pcap, beg_nav, end_nav, val_nav, nav_val, nav_end, beg_date, end_date) %>%
    dplyr::group_by(pm_fund_id) %>%
      dplyr::filter(n() > 1) %>%
    dplyr::ungroup() # filters out funds with only one recorded date


  # nav_valdate represents nav at first date and valdate
  nav_val = nav_date_cutoffs %>%
    dplyr::filter(pcap == FALSE) %>%
    dplyr::rename(nav_cutoff = nav_val) %>%
    dplyr::select(pm_fund_id, effective_date, nav_cutoff, beg_date, end_date) %>%
    dplyr::mutate(pcap = FALSE)

  #nav_max represents nav at first date and valdate or max date if it exists (pcap)
  nav_end = nav_date_cutoffs %>%
    dplyr::filter(pcap == TRUE | effective_date == beg_date) %>%
    dplyr::group_by(pm_fund_id) %>% dplyr::filter(n() > 1) %>% dplyr::ungroup() %>%
    dplyr::rename(nav_cutoff = nav_end) %>%
    dplyr::select(pm_fund_id, effective_date, nav_cutoff, beg_date, end_date) %>%
    dplyr::mutate(pcap = TRUE)

  nav_vals_by_date = dplyr::bind_rows(nav_val, nav_end) %>%
    dplyr::mutate(cash_flow = 0, contributions = 0, distributions = 0) # to simplify bind_rows (no NA)

  pcap_funds = nav_date_cutoffs %>%
    dplyr::filter(pcap == TRUE) %>%
    dplyr::select(pm_fund_id, beg_date, end_date) %>%
    unique()

  no_pcap_funds = nav_date_cutoffs %>%
    dplyr::filter(pcap == FALSE) %>%
    dplyr::select(pm_fund_id, beg_date, end_date) %>%
    unique()

  # Combine nav & cf_bench --> after join, data with a pcap will be duplicated (must always filter or group by pcap)
  # TODO: drop cash flow if it occurs on same first date of nav
  nav_cf_daily_val = no_pcap_funds %>%
    dplyr::left_join(cf_bench_daily, by = 'pm_fund_id') %>%
    dplyr::mutate(nav_cutoff = 0, pcap = FALSE) %>%
    dplyr::bind_rows(nav_val) %>%
    dplyr::filter(effective_date >= beg_date) %>%
    dplyr::filter(effective_date <= valdate) %>%
    dplyr::group_by(pm_fund_id, effective_date, pcap) %>%
    dplyr::summarize(cash_flow = sum(cash_flow),
                     nav_cutoff = sum(nav_cutoff),
                     contributions = sum(contributions),
                     distributions = sum(distributions)) %>%
    dplyr::ungroup() %>%
    drop_na(effective_date)
  nav_cf_daily_val[is.na(nav_cf_daily_val)] = 0

  nav_cf_daily_end = pcap_funds %>%
    dplyr::left_join(cf_bench_daily, by = 'pm_fund_id') %>%
    dplyr::mutate(nav_cutoff = 0, pcap = TRUE) %>%
    dplyr::bind_rows(nav_end) %>%
    dplyr::filter(effective_date >= beg_date) %>%
    dplyr::filter(effective_date <= end_date) %>%
    dplyr::group_by(pm_fund_id, effective_date, pcap) %>%
    dplyr::summarize(cash_flow = sum(cash_flow),
                     nav_cutoff = sum(nav_cutoff),
                     contributions = sum(contributions),
                     distributions = sum(distributions)) %>%
    dplyr::ungroup() %>%
    drop_na(effective_date)
  nav_cf_daily_end[is.na(nav_cf_daily_end)] = 0

  nav_cf_daily = dplyr::bind_rows(nav_cf_daily_val, nav_cf_daily_end) %>%
    dplyr::mutate(cash_flow_cutoff = cash_flow + nav_cutoff) %>%
    dplyr::left_join(bench_daily, by = c('pm_fund_id', 'effective_date')) %>%
    tidyr::drop_na(effective_date)
  nav_cf_daily[is.na(nav_cf_daily)] = 0


  #PME setup calcs
  # Get last
  fv_index_factors = nav_cf_daily %>%
    dplyr::filter(effective_date == date_cutoff | effective_date == valdate) %>%
    dplyr::select(pm_fund_id, index_value ,pcap) %>%
    dplyr::rename(last_index_value = index_value)

  # nav_first_dates = nav_cf_daily %>%
  #   dplyr::group_by(pm_fund_id) %>%
  #   dplyr::summarize(first_date = min(effective_date))

  nav_cf_w_fv = nav_cf_daily %>%
    dplyr::left_join(fv_index_factors, by = c('pm_fund_id', 'pcap'))
  nav_cf_w_fv[is.na(nav_cf_w_fv)] = 0

  final_data = nav_cf_w_fv %>%
    dplyr::left_join(pmfi, by = 'pm_fund_id') %>%
    dplyr::ungroup()

  # Allow for calc of 'TOTAL PM'
  final_data$TOTAL = 'TOTAL PM'

  # Calculate IRR, DPI, TVPI, Appreciation
  fund_metrics = final_data %>%
    dplyr::group_by(pcap, !!! group_vars, effective_date) %>%
    dplyr:: summarize(cash_flow_cutoff = sum(cash_flow_cutoff),
                      contributions = sum(contributions),
                      distributions = sum(distributions),
                      nav_cutoff = sum(nav_cutoff),
                      cash_flow = sum(cash_flow),
                      last_index_value = sum(last_index_value),
                      index_value = sum(index_value)) %>%
    dplyr::group_by(pcap, !!! group_vars) %>%
    dplyr::summarize(irr = calc_irr(cash_flow_cutoff, effective_date),
                     dpi = calc_dpi(distributions, contributions),
                     tvpi = calc_tvpi(distributions, contributions, nav_cutoff),
                     appreciation = calc_appreciation(nav_cutoff, cash_flow),
                     pme = calc_pme(distributions, contributions, last_index_value/index_value, nav_cutoff)) %>%
    dplyr::ungroup()

  fund_metrics_false = fund_metrics %>% dplyr::filter(pcap == FALSE) %>% dplyr::select(-pcap)
  fund_metrics_true = fund_metrics %>% dplyr::filter(pcap == TRUE) %>% dplyr::select(!!! group_vars, irr, -pcap) %>% dplyr::rename(irr_pcap = irr)
  fund_metrics_final = fund_metrics_false %>% dplyr::left_join(fund_metrics_true, by = group_vars_char)

  return(fund_metrics_final)
}
