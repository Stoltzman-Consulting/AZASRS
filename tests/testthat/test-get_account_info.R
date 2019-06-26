context("test-get_account_info")

test_that("get_account_info() returns matching tibble", {
  account_info = get_account_info()
  expect_equal(account_info, readRDS_test(account_info))
})
