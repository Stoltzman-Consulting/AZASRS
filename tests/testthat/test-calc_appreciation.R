context("test-calc_appreciation")

test_that("calc_appreciation() returns expected appreciation", {
  cash_flow = c(-100, 10, 20, 10, 160)
  nav = c(-50, 0, 0, 0, 200)
  appreciation = calc_appreciation(cash_flow, nav)
  appreciation_expected = 50
  expect_equal(appreciation, appreciation_expected, tolerance = 1e-6)
})
