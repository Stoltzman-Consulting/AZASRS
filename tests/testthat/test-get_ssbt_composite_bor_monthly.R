context("test-get_ssbt_composite_bor_monthly")

test_that("get_ssbt_composite_bor_monthly() returns matching tibble", {
  ssbt_composite_bor_monthly = get_ssbt_composite_bor_monthly()
  expect_equal(ssbt_composite_bor_monthly, readRDS_test(ssbt_composite_bor_monthly))
})
