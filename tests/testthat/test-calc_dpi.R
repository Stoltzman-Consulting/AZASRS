context("test-calc_dpi")

test_that("calc_dpi() returns expected dpi", {
  distributions = c(-100, -50, -30)
  contributions = c(20, 10, 5)
  dpi = calc_dpi(distributions, contributions)
  dpi_expected = 5.142857
  expect_equal(dpi, dpi_expected, tolerance = 1e-6)
})
