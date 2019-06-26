context("test-calc_pme")

test_that("calc_pme() returns accurate pme", {
  distributions = c(-100, -50, -30)
  contributions = c(20, 10, 5)
  nav = c(-50, 0, 200)
  fv_index_factors = c(1.2, 1.1, 1)
  pme = calc_pme(distributions, contributions, nav, fv_index_factors)
  pme_expected = -1.375
  expect_equal(pme, pme_expected, tolerance = 1e-6)
})
