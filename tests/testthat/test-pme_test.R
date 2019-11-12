
test_that("P2P PME is equivalent", {

  start_date = '2018-12-31'
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

  test_data_final = test_data_clean %>%
    dplyr::mutate(benchmark_description = stringr::str_replace_all(benchmark_description, '.PME', '')) %>%
    dplyr::mutate(benchmark_description = stringr::str_replace_all(benchmark_description, ' ', ''))


  con = AZASRS_DATABASE_CONNECTION()

  pmfi = tbl_view_all_pm_fund_info(con)
  bi = tbl_benchmark_info(con)

  nav = get_pm_nav_daily(con = con, return_tibble = FALSE) %>%
    dplyr::filter(effective_date >= start_date)

  cf = get_pm_cash_flow_daily(con = con, return_tibble = FALSE) %>%
    dplyr::filter(effective_date >= start_date)

  benchmark_index = get_benchmark_daily_index(con = con, return_tibble = FALSE) %>%
    dplyr::filter(effective_date >= start_date,
                  effective_date <= end_date) %>%
    dplyr::arrange(effective_date)

  benchmark_fv = benchmark_index %>%
    dplyr::group_by(benchmark_info_id) %>%
    dplyr::mutate(fv_index_factor = index_value / dplyr::first(index_value)) %>%
    dplyr::ungroup()

  fv_index_factors = benchmark_fv %>%
    dplyr::distinct(benchmark_info_id, effective_date) %>%
    dplyr::filter(effective_date == start_date | effective_date == end_date) #%>%
    # dplyr::select(benchmark_info_id, index_value) %>%
    # dplyr::rename(last_index_value = index_value)

  # benchmark_fv %>%
  #   dplyr::left_join(bi, by = 'benchmark_info_id') %>%
  #   dplyr::left_join(pmfi, by = 'pm_fund_info_id')
#
#   bi_min_dates = bi %>%
#     dplyr::group_by(benchmark_description) %>%
#     dplyr::mutate(min_date = min(effective_date)) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(first_index_value = dplyr::if_else(
#       effective_date == min_date,
#       index_value,
#       0)) %>%
#     dplyr::group_by(benchmark_description) %>%
#     dplyr::mutate(first_index_value = max(first_index_value)) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(fv_index_factor = index_value / first_index_value) %>%
#     dplyr::select(benchmark_description, effective_date, fv_index_factor)

  # pmfi_bi = tbl_pm_fund_info_benchmark_info(con) %>%
  #   dplyr::left_join(bi, by = 'benchmark_info_id') %>%
  #   dplyr::left_join(tbl_benchmark_type(con) %>%
  #                      dplyr::filter(benchmark_type == 'PVT'),
  #                    by = 'benchmark_type_id') %>%
  #   dplyr::left_join(pmfi, by = 'pm_fund_info_id') %>%
  #   dplyr::select(pm_fund_description, benchmark_description, benchmark_id, benchmark_info_id) #%>%
    # dplyr::filter(benchmark_type == 'IMP') %>%
    # tibble::as_tibble() %>%
    # dplyr::mutate(pm_fund_description = stringr::str_replace_all(pm_fund_description, ' ', '.'),
    #               benchmark_description = stringr::str_replace_all(benchmark_description, ' ', '.'))

  # unique_bii = pmfi_bi %>%
  #   select(benchmark_info_id) %>%
  #   distinct()

  # bi = tbl_benchmark_daily_index(con) %>%
  #   dplyr::left_join(tbl_pm_fund_info_benchmark_info(con), by = 'benchmark_info_id') %>%
  #   dplyr::left_join(tbl_benchmark_type(con), by = 'benchmark_type_id') %>%
  #   dplyr::left_join(tbl_benchmark_info(con), by = 'benchmark_info_id') %>%
  #   dplyr::filter(effective_date >= start_date) %>%
  #   # dplyr::filter(benchmark_type == 'IMP') %>%
  #   dplyr::filter(benchmark_info_id %in% unique_bii) %>%
  #   dplyr::distinct(benchmark_description, effective_date, index_value) %>%
  #   tibble::as_tibble() %>%
  #   dplyr::mutate(benchmark_description = stringr::str_replace_all(benchmark_description, ' ', '.'))


  # nav = get_pm_nav_daily(con = con, return_tibble = FALSE) %>%
  #   dplyr::filter(effective_date >= start_date) %>%
  #   tibble::as_tibble()
  #
  # cf = get_pm_cash_flow_daily(con = con, return_tibble = FALSE) %>%
  #   dplyr::filter(effective_date >= start_date) %>%
  #   tibble::as_tibble()

#   pm_bi_bi = pm_bi %>%
#     dplyr::left_join(bi)

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
    dplyr::left_join(pm_bi, by = 'pm_fund_description') %>%
    select(pm_fund_description, pme)

  b = pmfi %>%
    dplyr::left_join(test_data_clean, by = 'pm_fund_description') %>%
    select(pm_fund_description, pme)

  expect_equal(2 * 2, 4)

  # dplyr::mutate(benchmark_description_caps = toupper(gsub('[[:punct:] ]+','', benchmark_description_caps)),
  #               pm_fund_description_caps = toupper(gsub('[[:punct:] ]+','', pm_fund_description)))
})
























test_that("P2P IRR is equivalent", {

  con = AZASRS_DATABASE_CONNECTION()

  start_dt = '2018-03-31'
  end_dt = '2019-06-30'
  valdate = '2019-06-30'
  output_filename = 'data/irr_test_1_year.csv'

  pmfi = get_pm_fund_info(con = con, return_tibble = FALSE)
  nav = get_pm_nav_daily(con = con, return_tibble = FALSE)
  cf = get_pm_cash_flow_daily(con = con, return_tibble = FALSE)

  metrics = build_privm_metrics(pm_fund_portfolio, pm_fund_description,
                             start_date = start_dt,
                             pcap_date = end_dt,
                             value_date = end_dt)

  irrs = build_privm_p2p_irr(start_date = start_dt, end_date = end_dt, con = con)

  fund_irrs = irrs %>% dplyr::filter(grouping_type == 'pm_fund_description') %>%
    dplyr::transmute(pm_fund_description = name, irr = irr)

  cf_young_funds = cf %>%
    dplyr::group_by(pm_fund_description) %>%
    dplyr::mutate(min_date = min(effective_date)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(effective_date == min_date) %>%
    dplyr::mutate(time_since_start = effective_date - lubridate::as_date(valdate)) %>%
    dplyr::mutate(less_than_one_year_old = dplyr::if_else(abs(time_since_start) < 365, TRUE, FALSE)) %>%
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
    dplyr::left_join(irrs, by = 'pm_fund_description')
  b = a %>%
    dplyr::select(pm_fund_description, `3 Year IRR`, irr) %>%
    dplyr::mutate(irr = round(100*irr, 2),
                  irr_diff = (`3 Year IRR` - irr)) %>%
    dplyr::arrange(-abs(irr_diff)) %>%
    dplyr::left_join(cf_young_funds) %>%
    dplyr::left_join(pmfi %>% tibble::as_tibble() %>% dplyr::select(pm_fund_description, closed))
  b

  running_tibble = b


  dist_debt_test = readr::read_csv('data/irr_test/Other Credit Performance.csv') %>%
    dplyr::filter(Contributions > 0)
  dist_debt_nm = dist_debt_test %>% dplyr::select(`Investment Name`)
  dist_debt_irrs = dist_debt_test[, grep(" IRR", colnames(dist_debt_test))]
  dist_debt_test_irr = dplyr::bind_cols(dist_debt_nm, dist_debt_irrs) %>%
    dplyr::filter(!is.na(`Investment Name`)) %>%
    dplyr::rename(pm_fund_description = `Investment Name`)

  a = dist_debt_test_irr %>%
    dplyr::left_join(itd_irrs, by = 'pm_fund_description')
  b = a %>%
    dplyr::select(pm_fund_description, `3 Year IRR`, irr) %>%
    dplyr::mutate(irr = round(100*irr, 2),
                  irr_diff = (`3 Year IRR` - irr)) %>%
    dplyr::arrange(-abs(irr_diff)) %>%
    dplyr::left_join(cf_young_funds) %>%
    dplyr::left_join(pmfi %>% tibble::as_tibble() %>% dplyr::select(pm_fund_description, closed))
  b

  running_tibble = dplyr::bind_rows(running_tibble, b)


  dist_debt_test = readr::read_csv('data/irr_test/Private Debt Performance.csv') %>%
    dplyr::filter(Contributions > 0)
  dist_debt_nm = dist_debt_test %>% dplyr::select(`Investment Name`)
  dist_debt_irrs = dist_debt_test[, grep(" IRR", colnames(dist_debt_test))]
  dist_debt_test_irr = dplyr::bind_cols(dist_debt_nm, dist_debt_irrs) %>%
    dplyr::filter(!is.na(`Investment Name`)) %>%
    dplyr::rename(pm_fund_description = `Investment Name`)

  a = dist_debt_test_irr %>%
    dplyr::left_join(itd_irrs, by = 'pm_fund_description')
  b = a %>%
    dplyr::select(pm_fund_description, `3 Year IRR`, irr) %>%
    dplyr::mutate(irr = round(100*irr, 2),
                  irr_diff = (`3 Year IRR` - irr)) %>%
    dplyr::arrange(-abs(irr_diff)) %>%
    dplyr::left_join(cf_young_funds) %>%
    dplyr::left_join(pmfi %>% tibble::as_tibble() %>% dplyr::select(pm_fund_description, closed))
  b

  running_tibble = dplyr::bind_rows(running_tibble, b)

  dist_debt_test = readr::read_csv('data/irr_test/Private Equity Performance.csv') %>%
    dplyr::filter(Contributions > 0)
  dist_debt_nm = dist_debt_test %>% dplyr::select(`Investment Name`)
  dist_debt_irrs = dist_debt_test[, grep(" IRR", colnames(dist_debt_test))]
  dist_debt_test_irr = dplyr::bind_cols(dist_debt_nm, dist_debt_irrs) %>%
    dplyr::filter(!is.na(`Investment Name`)) %>%
    dplyr::rename(pm_fund_description = `Investment Name`)

  a = dist_debt_test_irr %>%
    dplyr::left_join(itd_irrs, by = 'pm_fund_description')
  b = a %>%
    dplyr::select(pm_fund_description, `3 Year IRR`, irr) %>%
    dplyr::mutate(irr = round(100*irr, 2),
                  irr_diff = (`3 Year IRR` - irr)) %>%
    dplyr::arrange(-abs(irr_diff)) %>%
    dplyr::left_join(cf_young_funds) %>%
    dplyr::left_join(pmfi %>% tibble::as_tibble() %>% dplyr::select(pm_fund_description, closed))
  b

  running_tibble = dplyr::bind_rows(running_tibble, b)

  dist_debt_test = readr::read_csv('data/irr_test/Real Estate Agg. Performance.csv') %>%
    dplyr::filter(Contributions > 0)
  dist_debt_nm = dist_debt_test %>% dplyr::select(`Investment Name`)
  dist_debt_irrs = dist_debt_test[, grep(" IRR", colnames(dist_debt_test))]
  dist_debt_test_irr = dplyr::bind_cols(dist_debt_nm, dist_debt_irrs) %>%
    dplyr::filter(!is.na(`Investment Name`)) %>%
    dplyr::rename(pm_fund_description = `Investment Name`)

  a = dist_debt_test_irr %>%
    dplyr::left_join(itd_irrs, by = 'pm_fund_description')
  b = a %>%
    dplyr::select(pm_fund_description, `3 Year IRR`, irr) %>%
    dplyr::mutate(irr = round(100*irr, 2),
                  irr_diff = (`3 Year IRR` - irr)) %>%
    dplyr::arrange(-abs(irr_diff)) %>%
    dplyr::left_join(cf_young_funds) %>%
    dplyr::left_join(pmfi %>% tibble::as_tibble() %>% dplyr::select(pm_fund_description, closed))
  b

  running_tibble = dplyr::bind_rows(running_tibble, b)

  haha = running_tibble %>% dplyr::arrange(-abs(irr_diff)) %>% dplyr::filter(`3 Year IRR` != 0)

  running_tibble %>% readr::write_csv(output_filename)

  expect_equal(2 * 2, 4)
})


#### nav should never be 0 before first cash flow (remove nav) ---> should be added to data-pipeline
#### if nav & cash flow on same day, only use nav (ignore cash flow)
#### if first cf is before first nav, then cf should be used as first nav
#### interactive view changes name of tab --> need to fix data-pipeline if changes
