context("test-pm_fund_data")

test_that("get_pm_nav_daily() matches test data", {
  new_dat = get_pm_nav_daily()
  hist_dat = readRDS(paste0(AZASRS_TEST_DATA_DIRECTORY, 'pm_nav_daily.rds'))
  expect_equal(new_dat, hist_dat)
})
