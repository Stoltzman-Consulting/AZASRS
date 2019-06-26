context("test-get_next_quarter")

test_that("get_next_quarter() returns matching next_quarter date", {
  next_quarter = get_next_quarter()
  expect_equal(next_quarter, readRDS_test(next_quarter))
})
