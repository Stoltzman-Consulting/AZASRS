context("test-build_privm_metrics")

test_that("build_privm_metrics() returns matching tibble", {
  privm_metrics = build_privm_metrics(pm_fund_portfolio, pm_fund_id)
  expect_equal(privm_metrics, readRDS_test(privm_metrics))
})
