context("test-get_account_bor_monthly")

test_that("get_account_bor_monthly() returns matching tibble", {
  account_bor_monthly = get_account_bor_monthly()
  expect_equal(account_bor_monthly, readRDS_test(account_bor_monthly))
})
