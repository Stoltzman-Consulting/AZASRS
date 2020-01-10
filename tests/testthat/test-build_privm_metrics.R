context("test-build_privm_metrics")

test_that("build_privm_metrics() returns matching tibble", {
  build_privm_metrics = build_privm_metrics(pm_fund_portfolio, pm_fund_id, start_date = '2016-12-31', value_date = '2019-03-31')
  build_privm_metrics_head = build_privm_metrics %>% head(3)
  build_privm_metrics_tail = build_privm_metrics %>% tail(3)

  expect_equal(build_privm_metrics_head, readRDS_test(build_privm_metrics_head))
  expect_equal(build_privm_metrics_tail, readRDS_test(build_privm_metrics_tail))
})
