context("test-build_get_url")

test_that("build_get_url() returns character string of length 1", {
  url_to_use <- build_get_url("pm_fund_info")

  expect_equal(class(url_to_use), "character")

  expect_equal(length(url_to_use), 1)

  expect_error(build_get_url())
})
