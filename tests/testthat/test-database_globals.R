# Database globals return proper data structures

test_that("Database connection", {
  con = AZASRS_DATABASE_CONNECTION()
  print('here')
  expect_equal(con@info$dbms.name, "Microsoft SQL Server")
  AZASRS_DATABASE_DISCONNECT(con)
})

test_that("Database disconnect", {
  con = AZASRS_DATABASE_CONNECTION()
  expect_equal(con@info$dbms.name, "Microsoft SQL Server")
  dis_con = AZASRS_DATABASE_DISCONNECT(con)
  expect_true(dis_con)
})

test_that("Proper value date (valdate)", {
  valdate = get_value_date()
  expect_equal(valdate, "2019-09-30")
})

test_that("Next quarter is one after valdate", {
  next_quarter = get_next_quarter()
  expect_equal(next_quarter, "2019-12-31")
})

test_that("PM Fund Info has not changed", {
  tbl_pm_fund_info = tbl_pm_fund_info() %>% tibble::as_tibble()
  expect_equal(tbl_pm_fund_info, readRDS_test(tbl_pm_fund_info))
})
