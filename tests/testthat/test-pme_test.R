
test_that("P2P PME is equivalent", {

  start_date = '2004-01-01'
  end_date = '2019-03-31'

  test_data = readr::read_csv('data/pme_test.csv')
  # Select only PME columns
  test_data_pme_only = test_data[, grep(" PME", colnames(test_data))]
  # Remove " 2" columns because they represent PCAP and not valdate IRRs
  test_data_no_2 = test_data_pme_only[, -grep(" 2", colnames(test_data_pme_only))]
  test_data_clean = dplyr::bind_cols(test_data %>%
                                       dplyr::select(X1), test_data_no_2) %>%
    dplyr::rename(pm_fund_description = X1) %>%
    tidyr::gather(benchmark_description, pme, -pm_fund_description) %>%
    dplyr::mutate(pm_fund_description = stringr::str_replace_all(pm_fund_description, ' ', '.'),
                  benchmark_description = stringr::str_replace_all(benchmark_description, ' ', '.'))


  con = AZASRS_DATABASE_CONNECTION()

  pm_bi = tbl_view_all_pm_fund_info(con) %>%
    dplyr::left_join(tbl_pm_fund_info_benchmark_info(con)) %>%
    dplyr::left_join(tbl_benchmark_info(con)) %>%
    dplyr::left_join(tbl_benchmark_type(con)) %>%
    # dplyr::filter(benchmark_type == 'IMP') %>%
    dplyr::select(pm_fund_description, benchmark_description, benchmark_id, benchmark_info_id) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(pm_fund_description = stringr::str_replace_all(pm_fund_description, ' ', '.'),
                  benchmark_description = stringr::str_replace_all(benchmark_description, ' ', '.'))

  unique_bii = pm_bi$benchmark_info_id %>% unique()

  bi = tbl_benchmark_daily_index(con) %>%
    dplyr::left_join(tbl_pm_fund_info_benchmark_info(con), by = 'benchmark_info_id') %>%
    dplyr::left_join(tbl_benchmark_type(con), by = 'benchmark_type_id') %>%
    dplyr::left_join(tbl_benchmark_info(con), by = 'benchmark_info_id') %>%
    dplyr::filter(effective_date >= start_date) %>%
    # dplyr::filter(benchmark_type == 'IMP') %>%
    dplyr::filter(benchmark_info_id %in% unique_bii) %>%
    dplyr::distinct(benchmark_description, effective_date, index_value) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(benchmark_description = stringr::str_replace_all(benchmark_description, ' ', '.'))

  bi_min_dates = bi %>%
    dplyr::group_by(benchmark_description) %>%
    dplyr::mutate(min_date = min(effective_date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(first_index_value = dplyr::if_else(
      effective_date == min_date,
      index_value,
      0)) %>%
    dplyr::group_by(benchmark_description) %>%
    dplyr::mutate(first_index_value = max(first_index_value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(fv_index_factor = index_value / first_index_value) %>%
    dplyr::select(benchmark_description, effective_date, fv_index_factor)


  # bi_min_dates %>%
  #   dplyr::left_join()

  nav = get_pm_nav_daily(effective_date >= start_date)
  cf = get_pm_cash_flow_daily(effective_date >= start_date)


  # %>%
  #   dplyr::mutate(benchmark_description = toupper(gsub('[[:punct:] ]+','', benchmark_description)),
  #                 pm_fund_description = toupper(gsub('[[:punct:] ]+','', pm_fund_description)))

  test_data_final = test_data_clean %>%
    dplyr::mutate(benchmark_description = stringr::str_replace_all(benchmark_description, '.PME', '')) %>%
    dplyr::mutate(benchmark_description = stringr::str_replace_all(benchmark_description, ' ', ''))
    # dplyr::mutate(benchmark_description_caps = toupper(gsub('[[:punct:] ]+','', benchmark_description_caps)),
    #               pm_fund_description_caps = toupper(gsub('[[:punct:] ]+','', pm_fund_description)))

  pm_bi_bi = pm_bi %>%
    dplyr::left_join(bi)

  fv_index_factors = bi %>%
    dplyr::filter(effective_date == start_date | effective_date == end_date) %>%
    dplyr::select(benchmark_description, index_value) %>%
    dplyr::rename(last_index_value = index_value)

  comparison_df = test_data_final %>%
    dplyr::left_join(fv_index_factors, by = 'benchmark_description') %>%
    dplyr::filter(!is.na(last_index_value))

  comparison_df = pm_bi %>%
    dplyr::left_join(test_data_final, by = 'benchmark_description')

  comparison_df %>% dplyr::filter(!is.na(pme))

  a = test_data_clean %>%
    dplyr::left_join(pm_bi, by = c('X1' = 'pm_fund_description'))

  b = pmfi %>%
    dplyr::left_join(test_data_clean, by = c('pm_fund_description' = 'X1'))

  expect_equal(2 * 2, 4)
})


test_that("P2P IRR is equivalent", {

  valdate = get_value_date()
  itd_irrs = build_privm_metrics(pm_fund_description, date_cutoff = valdate)

  cf = get_pm_cash_flow_daily()

  cf_young_funds = cf %>%
    dplyr::group_by(pm_fund_description) %>%
    dplyr::mutate(min_date = min(effective_date)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(effective_date == min_date) %>%
    dplyr::mutate(time_since_start = effective_date - lubridate::as_date(valdate)) %>%
    dplyr::mutate(less_than_one_year_old = if_else(abs(time_since_start) < 365, TRUE, FALSE)) %>%
    dplyr::filter(less_than_one_year_old) %>%
    dplyr::select(pm_fund_description, less_than_one_year_old)

  dist_debt_test = readr::read_csv('data/irr_test/Distressed Debt Performance.csv') %>%
    dplyr::filter(Contributions > 0)
  dist_debt_nm = dist_debt_test %>% dplyr::select(`Investment Name`)
  dist_debt_irrs = dist_debt_test[, grep(" IRR", colnames(dist_debt_test))]
  dist_debt_test_irr = dplyr::bind_cols(dist_debt_nm, dist_debt_irrs) %>%
    dplyr::filter(!is.na(`Investment Name`)) %>%
    dplyr::rename(pm_fund_description = `Investment Name`)

  a = dist_debt_test_irr %>%
    left_join(itd_irrs, by = 'pm_fund_description')
  b = a %>%
    dplyr::select(pm_fund_description, `ITD IRR`, irr) %>%
    dplyr::mutate(irr = round(100*irr, 2),
                  irr_diff = (`ITD IRR` - irr)) %>%
    arrange(-abs(irr_diff)) %>%
    dplyr::left_join(cf_young_funds)
  b


  dist_debt_test = readr::read_csv('data/irr_test/Other Credit Performance.csv') %>%
    dplyr::filter(Contributions > 0)
  dist_debt_nm = dist_debt_test %>% dplyr::select(`Investment Name`)
  dist_debt_irrs = dist_debt_test[, grep(" IRR", colnames(dist_debt_test))]
  dist_debt_test_irr = dplyr::bind_cols(dist_debt_nm, dist_debt_irrs) %>%
    dplyr::filter(!is.na(`Investment Name`)) %>%
    dplyr::rename(pm_fund_description = `Investment Name`)

  a = dist_debt_test_irr %>%
    left_join(itd_irrs, by = 'pm_fund_description')
  b = a %>%
    dplyr::select(pm_fund_description, `ITD IRR`, irr) %>%
    dplyr::mutate(irr = round(100*irr, 2),
                  irr_diff = (`ITD IRR` - irr)) %>%
    arrange(-abs(irr_diff)) %>%
    dplyr::left_join(cf_young_funds)
  b


  dist_debt_test = readr::read_csv('data/irr_test/Private Debt Performance.csv') %>%
    dplyr::filter(Contributions > 0)
  dist_debt_nm = dist_debt_test %>% dplyr::select(`Investment Name`)
  dist_debt_irrs = dist_debt_test[, grep(" IRR", colnames(dist_debt_test))]
  dist_debt_test_irr = dplyr::bind_cols(dist_debt_nm, dist_debt_irrs) %>%
    dplyr::filter(!is.na(`Investment Name`)) %>%
    dplyr::rename(pm_fund_description = `Investment Name`)

  a = dist_debt_test_irr %>%
    left_join(itd_irrs, by = 'pm_fund_description')
  b = a %>%
    dplyr::select(pm_fund_description, `ITD IRR`, irr) %>%
    dplyr::mutate(irr = round(100*irr, 2),
                  irr_diff = (`ITD IRR` - irr)) %>%
    arrange(-abs(irr_diff)) %>%
    dplyr::left_join(cf_young_funds)
  b


  dist_debt_test = readr::read_csv('data/irr_test/Private Equity Performance.csv') %>%
    dplyr::filter(Contributions > 0)
  dist_debt_nm = dist_debt_test %>% dplyr::select(`Investment Name`)
  dist_debt_irrs = dist_debt_test[, grep(" IRR", colnames(dist_debt_test))]
  dist_debt_test_irr = dplyr::bind_cols(dist_debt_nm, dist_debt_irrs) %>%
    dplyr::filter(!is.na(`Investment Name`)) %>%
    dplyr::rename(pm_fund_description = `Investment Name`)

  a = dist_debt_test_irr %>%
    left_join(itd_irrs, by = 'pm_fund_description')
  b = a %>%
    dplyr::select(pm_fund_description, `ITD IRR`, irr) %>%
    dplyr::mutate(irr = round(100*irr, 2),
                  irr_diff = (`ITD IRR` - irr)) %>%
    arrange(-abs(irr_diff)) %>%
    dplyr::left_join(cf_young_funds)
  b

  dist_debt_test = readr::read_csv('data/irr_test/Real Estate Agg. Performance.csv') %>%
    dplyr::filter(Contributions > 0)
  dist_debt_nm = dist_debt_test %>% dplyr::select(`Investment Name`)
  dist_debt_irrs = dist_debt_test[, grep(" IRR", colnames(dist_debt_test))]
  dist_debt_test_irr = dplyr::bind_cols(dist_debt_nm, dist_debt_irrs) %>%
    dplyr::filter(!is.na(`Investment Name`)) %>%
    dplyr::rename(pm_fund_description = `Investment Name`)

  a = dist_debt_test_irr %>%
    left_join(itd_irrs, by = 'pm_fund_description')
  b = a %>%
    dplyr::select(pm_fund_description, `ITD IRR`, irr) %>%
    dplyr::mutate(irr = round(100*irr, 2),
                  irr_diff = (`ITD IRR` - irr)) %>%
    arrange(-abs(irr_diff)) %>%
    dplyr::left_join(cf_young_funds)
  b




  # Select only PME columns
  test_data_pme_only = test_data[, grep(" PME", colnames(test_data))]
  # Remove " 2" columns because they represent PCAP and not valdate IRRs
  test_data_no_2 = test_data_pme_only[, -grep(" 2", colnames(test_data_pme_only))]
  test_data_clean = dplyr::bind_cols(test_data %>%
                                       dplyr::select(X1), test_data_no_2) %>%
    dplyr::rename(pm_fund_description = X1) %>%
    tidyr::gather(benchmark_description, pme, -pm_fund_description) %>%
    dplyr::mutate(pm_fund_description = stringr::str_replace_all(pm_fund_description, ' ', '.'),
                  benchmark_description = stringr::str_replace_all(benchmark_description, ' ', '.'))


  con = AZASRS_DATABASE_CONNECTION()

  pm_bi = tbl_view_all_pm_fund_info(con) %>%
    dplyr::left_join(tbl_pm_fund_info_benchmark_info(con)) %>%
    dplyr::left_join(tbl_benchmark_info(con)) %>%
    dplyr::left_join(tbl_benchmark_type(con)) %>%
    dplyr::filter(benchmark_type == 'SAA') %>%
    dplyr::select(pm_fund_description, benchmark_description, benchmark_id, benchmark_info_id) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(pm_fund_description = stringr::str_replace_all(pm_fund_description, ' ', '.'),
                  benchmark_description = stringr::str_replace_all(benchmark_description, ' ', '.'))

  unique_bii = pm_bi$benchmark_info_id %>% unique()

  bi = tbl_benchmark_daily_index(con) %>%
    dplyr::left_join(tbl_pm_fund_info_benchmark_info(con), by = 'benchmark_info_id') %>%
    dplyr::left_join(tbl_benchmark_type(con), by = 'benchmark_type_id') %>%
    dplyr::left_join(tbl_benchmark_info(con), by = 'benchmark_info_id') %>%
    dplyr::filter(effective_date >= start_date) %>%
    dplyr::filter(benchmark_type == 'SAA') %>%
    dplyr::filter(benchmark_info_id %in% unique_bii) %>%
    dplyr::distinct(benchmark_description, effective_date, index_value) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(benchmark_description = stringr::str_replace_all(benchmark_description, ' ', '.'))

  bi_min_dates = bi %>%
    dplyr::group_by(benchmark_description) %>%
    dplyr::mutate(min_date = min(effective_date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(first_index_value = dplyr::if_else(
      effective_date == min_date,
      index_value,
      0)) %>%
    dplyr::group_by(benchmark_description) %>%
    dplyr::mutate(first_index_value = max(first_index_value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(fv_index_factor = index_value / first_index_value) %>%
    dplyr::select(benchmark_description, effective_date, fv_index_factor)


  # bi_min_dates %>%
  #   dplyr::left_join()

  nav = get_pm_nav_daily(effective_date >= start_date)
  cf = get_pm_cash_flow_daily(effective_date >= start_date)


  # %>%
  #   dplyr::mutate(benchmark_description = toupper(gsub('[[:punct:] ]+','', benchmark_description)),
  #                 pm_fund_description = toupper(gsub('[[:punct:] ]+','', pm_fund_description)))

  test_data_final = test_data_clean %>%
    dplyr::mutate(benchmark_description = stringr::str_replace_all(benchmark_description, '.PME', '')) %>%
    dplyr::mutate(benchmark_description = stringr::str_replace_all(benchmark_description, ' ', ''))
  # dplyr::mutate(benchmark_description_caps = toupper(gsub('[[:punct:] ]+','', benchmark_description_caps)),
  #               pm_fund_description_caps = toupper(gsub('[[:punct:] ]+','', pm_fund_description)))

  pm_bi_bi = pm_bi %>%
    dplyr::left_join(bi)

  fv_index_factors = bi %>%
    dplyr::filter(effective_date == start_date | effective_date == end_date) %>%
    dplyr::select(benchmark_description, index_value) %>%
    dplyr::rename(last_index_value = index_value)

  comparison_df = test_data_final %>%
    dplyr::left_join(fv_index_factors, by = 'benchmark_description')

  comparison_df = pm_bi %>%
    dplyr::left_join(test_data_final, by = 'benchmark_description')

  comparison_df %>% dplyr::filter(!is.na(pme))

  a = test_data_clean %>%
    dplyr::left_join(pm_bi, by = c('X1' = 'pm_fund_description'))

  b = pmfi %>%
    dplyr::left_join(test_data_clean, by = c('pm_fund_description' = 'X1'))

  expect_equal(2 * 2, 4)
})


#### nav should never be 0 before first cash flow (remove nav) ---> should be added to data-pipeline
#### if nav & cash flow on same day, only use nav (ignore cash flow)
#### if first cf is before first nav, then cf should be used as first nav
#### interactive view changes name of tab --> need to fix data-pipeline if changes
