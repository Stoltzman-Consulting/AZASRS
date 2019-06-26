context("test-get_value_date")

test_that("get_value_date() returns matching valdate", {
  value_date = get_value_date()
  expect_equal(value_date, readRDS_test(value_date))
})
