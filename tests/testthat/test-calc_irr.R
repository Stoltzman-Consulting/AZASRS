context("test-calc_irr")

test_that("calc_irr() returns matching calculation for irr", {
  cash_flows = c(-100, 10, 20, 10, 160)
  dates = as.Date(c('2018-01-01', '2018-04-02', '2018-06-28', '2018-07-30', '2018-12-31'), format = '%Y-%m-%d')
  irr = asrsMethods::irr.z(zoo::zoo(cash_flows, dates))
  irr_expected = 1.228008
  expect_equal(irr, irr_expected, tolerance = 1e-6)
})

