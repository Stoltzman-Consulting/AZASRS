test_that("filled_list_of_dates matches", {
  filled_list_of_dates = filled_list_of_dates(start_date = '2004-12-31', end_date = '2019-03-31', time_delta = 'quarters')
  expect_equal(filled_list_of_dates, readRDS_test(filled_list_of_dates))
})
