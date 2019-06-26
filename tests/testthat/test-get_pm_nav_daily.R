context("test-get_pm_nav_daily")

test_that("get_pm_nav_daily() matches test data", {
  pm_nav_daily = get_pm_nav_daily()
  historical_dat = readRDS_test(pm_nav_daily)
  expect_equal(pm_nav_daily, historical_dat)
})
