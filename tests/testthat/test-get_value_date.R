context("test-get_value_date")

test_that("get_value_date() returns string of date, length 1", {
  test_date <- get_value_date()

  expect_equal(class(test_date), "Date")

  expect_equal(length(test_date), 1)
})
