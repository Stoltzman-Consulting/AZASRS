context("test-get_total_fund_value")

test_that("get_total_fund_value() returns matching total_fund_value", {
  total_fund_value = get_total_fund_value()
  expect_equal(total_fund_value, readRDS_test(total_fund_value))
})
