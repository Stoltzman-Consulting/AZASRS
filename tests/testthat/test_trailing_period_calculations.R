context("Test the trailing period calculations returns proper data")

test_that("Database functions return correct columns and types", {

  trailing_pd_irr = trailing_period_irr(shortname = 'Total PE',
                                        portfolio_name = 'PE',
                                        comparison_years = c(1,3,5),
                                        benchmark_type = 'benchmark',
                                        benchmark_symbol = 'DEFAULT_FROM_FUNDINFO')
  expect_equal(trailing_pd_irr, TEST_DATA_trailing_period_irr)

  # How to recreate the test data
  # TEST_DATA_trailing_period_irr = trailing_period_irr(shortname = 'Total PE',
  #                                                     portfolio_name = 'PE',
  #                                                     comparison_years = c(1,3,5),
  #                                                     benchmark_type = 'benchmark',
  #                                                     benchmark_symbol = 'DEFAULT_FROM_FUNDINFO')
  # devtools::use_data(TEST_DATA_trailing_period_irr)
})
