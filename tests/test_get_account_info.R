context('Test function get_account_info')


test_that("Database returns proper column names and data types", {
  account_info_columns = sapply(get_account_info(), class)
  expect_equal(account_info_columns, test_account_info_columns)
  # to create test data
  # test_account_info_columns = account_info_columns
  # devtools::use_data(test_account_info_columns)
})

test_that("Database returns data filtered by multiple columns", {
  account_info_filtered = get_account_info(asset_class == 'Equities', category == 'Large')
  expect_equal(account_info_filtered, test_account_info_filtered)
})
