context("test-get_ssbt_composite_bor_daily")

test_that("get_ssbt_composite_bor_daily() returns matching tibble", {
  ssbt_composite_bor_daily = get_ssbt_composite_bor_daily()
  expect_equal(ssbt_composite_bor_daily, readRDS_test(ssbt_composite_bor_daily))
})
