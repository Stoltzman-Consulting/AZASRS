context("Test the database returns proper fields")

test_that("Database functions return correct columns and types", {
  fundinfo = pull_fundinfo()
  fundinfo_typeof = sapply(fundinfo,typeof)
  expect_equal(fundinfo_typeof, TEST_DATA_typeof$fundinfo)

  benchmark = pull_benchmark()
  benchmark_typeof = sapply(benchmark, typeof)
  expect_equal(benchmark_typeof, TEST_DATA_typeof$benchmark)

  cashflow = pull_cashflow()
  cashflow_typeof = sapply(cashflow, typeof)
  expect_equal(cashflow_typeof, TEST_DATA_typeof$cashflow)

  nav = pull_nav()
  nav_typeof = sapply(nav, typeof)
  expect_equal(nav_typeof, TEST_DATA_typeof$nav)

  # How to recreate the test data
  # TEST_DATA_typeof = setNames(list(fundinfo_typeof,
  #                    benchmark_typeof,
  #                    cashflow_typeof,
  #                    nav_typeof),
  #                    c('fundinfo', 'benchmark', 'cashflow', 'nav'))
  # devtools::use_data(TEST_DATA_typeof)
})


test_that("Valdate is up to date in first position", {
  fundinfo = pull_fundinfo()
  valdate = get_valdate()
  expect_equal(fundinfo$csdate[1], "12/31/2017") # finds it in the first position
  expect_equal(valdate, as.Date("2017-12-31")) # ensures it's converted to date
})

