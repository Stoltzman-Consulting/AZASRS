context("test-get_account_bor_daily")

test_that("get_account_bor_daily() returns matching tibble", {
  account_bor_daily = get_account_bor_daily()
  expect_equal(account_bor_daily, readRDS_test(account_bor_daily))
})
