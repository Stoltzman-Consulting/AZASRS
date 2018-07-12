context("Test the database returns proper fields")

test_that("Valdate is up to date in first position", {
  fundinfo = pull_fundinfo()
  valdate = get_valdate()
  expect_equal(fundinfo$csdate[1], "12/31/2017")
  expect_equal(valdate, "2017-12-31")
})

test_that("a equals b fails", {
  expect_equal('a','b')
})
