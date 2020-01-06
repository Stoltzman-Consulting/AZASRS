context("test-get_pm_cash_flow_daily")

test_that("get_pm_cash_flow_daily() returns matching tibbles", {
  cf = get_pm_cash_flow_daily()
  pm_cash_flow_daily_head = cf %>% head(3)
  pm_cash_flow_daily_tail = cf %>% tail(3)

  expect_equal(pm_cash_flow_daily_head, readRDS_test(pm_cash_flow_daily_head))
  expect_equal(pm_cash_flow_daily_tail, readRDS_test(pm_cash_flow_daily_tail))
})
