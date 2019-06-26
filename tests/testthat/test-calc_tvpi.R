context("test-calc_tvpi")

test_that("calc_tvpi() returns correct tvpi", {
  distributions = c(-100, -50, -30)
  contributions = c(20, 10, 5)
  nav = c(-50, 0, 200)
  tvpi = calc_tvpi(distributions, contributions, nav)
  tvpi_expected = -0.8571429
  expect_equal(tvpi, tvpi_expected, tolerance = 1e-6)
})
