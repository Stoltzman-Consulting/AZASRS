test_that("get_benchmark_daily_index() matches test data", {
  benchmark_daily_index = get_benchmark_daily_index() %>% tibble::as_tibble()
  expected_names = c("pm_fund_info_id", "effective_date", "nav", "pm_fund_id", "pm_fund_description",
                     "pm_fund_common_name", "vintage", "commit", "unfunded", "legacy",
                     "specialist", "invest_end", "term_end", "extension", "ext_time",
                     "ext_used", "fee_cat", "consultant", "adv_board", "obsvr", "fund_size_m",
                     "closed", "pm_fund_category", "pm_fund_category_description",
                     "pm_fund_portfolio", "pm_fund_sponsor", "pm_fund_city", "pm_fund_sector")

  expect_equal(colnames(pm_nav_daily), expected_names)
  expect_true(dplyr::count(pm_nav_daily) %>% dplyr::pull(n) > 6963)
})
