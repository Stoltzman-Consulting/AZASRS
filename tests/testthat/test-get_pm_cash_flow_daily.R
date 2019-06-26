context("test-get_pm_cash_flow_daily")

test_that("get_pm_cash_flow_daily() returns matching tibble", {
  pm_cash_flow_daily = get_pm_cash_flow_daily()
  expect_equal(pm_cash_flow_daily, readRDS_test(pm_cash_flow_daily))
})
