context("test-get_pm_fund_info")

test_that("get_pm_fund_info() returns matching tibble", {
  pm_fund_info = get_pm_fund_info()
  expect_equal(pm_fund_info, readRDS_test(pm_fund_info))
})
