context("test-get_ssbt_composite_info")

test_that("get_ssbt_composite_info() returns matching tibble", {
  ssbt_composite_info = get_ssbt_composite_info()
  expect_equal(ssbt_composite_info, readRDS_test(ssbt_composite_info))
})
