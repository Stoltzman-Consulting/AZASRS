# Database globals return proper data structures

test_that("Database connection returns proper connection type", {
  conn = AZASRS_DATABASE_CONNECTION()
  expect_equal(conn@info$dbms.name, "Microsoft SQL Server")
  AZASRS_DATABASE_DISCONNECT(conn)
})

test_that("Database connection name for both production and development", {
  # Default case returns production
  conn = AZASRS_DATABASE_CONNECTION()
  expect_equal(conn@info$dbname, Sys.getenv('DATABASE'))

  # development = 0 returns production
  conn = AZASRS_DATABASE_CONNECTION(development = 0)
  expect_equal(conn@info$dbname, Sys.getenv('DATABASE'))

  # development = 1 returns production
  conn = AZASRS_DATABASE_CONNECTION(development = 1)
  expect_equal(conn@info$dbname, Sys.getenv('DATABASE_DEVELOPMENT'))

})

test_that("Database disconnect returns TRUE", {
  conn = AZASRS_DATABASE_CONNECTION()
  dis_con = AZASRS_DATABASE_DISCONNECT(conn)
  expect_true(dis_con)
})

test_that("Proper value date (valdate)", {
  valdate = get_value_date()
  expect_equal(valdate, "2019-12-31")
})

test_that("Next quarter is one after valdate", {
  valdate = get_value_date()
  next_quarter = get_next_quarter()
  next_qtr_calculated = as.character(calc_add_qtrs(valdate, 1))
  expect_equal(next_quarter, "2020-03-31")
  expect_equal(next_quarter, next_qtr_calculated)
})

test_that("PM Fund Info columns have not changed", {
  tbl_pm_fund_info = tbl_pm_fund_info() %>% tibble::as_tibble()
  colnames_tbl_pm_fund_info = colnames(tbl_pm_fund_info)
  expected_colnames_tbl_pm_fund_info = c("pm_fund_info_id", "pm_fund_id", "pm_fund_description", "pm_fund_common_name",
    "pm_fund_portfolio_id", "pm_fund_category_id", "pm_fund_category_description_id",
    "pm_fund_sponsor_id", "pm_fund_city_id", "pm_fund_sector_id",
    "vintage", "commit", "unfunded", "legacy", "specialist", "invest_end",
    "term_end", "extension", "ext_time", "ext_used", "fee_cat", "consultant",
    "adv_board", "obsvr", "fund_size_m", "closed")
  expect_equal(colnames_tbl_pm_fund_info, expected_colnames_tbl_pm_fund_info)
})

test_that("tbl_* return proper values / types", {
  conn = AZASRS_DATABASE_CONNECTION()

  check_names = function(con, tbl_function, columns_expected){
    tmp = tbl_function(con)
    expect_equal(class(tmp)[1], "tbl_Microsoft SQL Server")
    expect_equal(colnames(tibble::tibble(tmp)), columns_expected)
  }

  check_names(con = conn, tbl_account_asset_class, c("account_asset_class_id", "account_asset_class"))
  check_names(con = conn, tbl_account_book_of_record_daily, c("account_info_id", "effective_date", "beginning_market_value", "ending_market_value", "net_cash_flow", "daily_return"))
  check_names(con = conn, tbl_account_book_of_record_monthly, c("account_info_id", "effective_date", "beginning_market_value", "ending_market_value", "net_cash_flow", "monthly_return", "fiscal_ytd_return"))
  check_names(con = conn, tbl_account_category, c("account_asset_class_id", "account_asset_class"))
  check_names(con = conn, tbl_account_info, c("account_asset_class_id", "account_asset_class"))
  check_names(con = conn, tbl_account_info_benchmark_info, c("account_asset_class_id", "account_asset_class"))
  check_names(con = conn, tbl_account_info_pm_fund_info, c("account_asset_class_id", "account_asset_class"))
  check_names(con = conn, tbl_account_portfolio, c("account_asset_class_id", "account_asset_class"))
  check_names(con = conn, tbl_account_sponsor, c("account_asset_class_id", "account_asset_class"))
  check_names(con = conn, tbl_account_sub_portfolio, c("account_asset_class_id", "account_asset_class"))
  check_names(con = conn, tbl_benchmark_daily_index, c("account_asset_class_id", "account_asset_class"))
  check_names(con = conn, tbl_benchmark_daily_return, c("account_asset_class_id", "account_asset_class"))
  check_names(con = conn, tbl_benchmark_info, c("account_asset_class_id", "account_asset_class"))
  check_names(con = conn, tbl_benchmark_monthly_return, c("benchmark_info_id", "effective_date", "monthly_return", "fiscal_ytd_return"))
  check_names(con = conn, tbl_benchmark_type, c("benchmark_type_id", "benchmark_type"))
  check_names(con = conn, tbl_constants, c("value_date", "next_quarter"))
  check_names(con = conn, tbl_pm_fund_cash_flow_daily, c("pm_fund_info_id", "effective_date", "cash_flow"))
  check_names(con = conn, tbl_pm_fund_category, c("pm_fund_category_id", "pm_fund_category"))
  check_names(con = conn, tbl_pm_fund_category_description, c("pm_fund_category_description_id", "pm_fund_category_description"))
  check_names(con = conn, tbl_pm_fund_city, c("pm_fund_city_id", "pm_fund_city"))
  check_names(con = conn, tbl_pm_fund_info, c("pm_fund_info_id", "pm_fund_id", "pm_fund_description", "pm_fund_common_name", "pm_fund_portfolio_id", "pm_fund_category_id", "pm_fund_category_description_id", "pm_fund_sponsor_id", "pm_fund_city_id", "pm_fund_sector_id", "vintage", "commit", "unfunded", "legacy", "specialist", "invest_end", "term_end", "extension", "ext_time", "ext_used", "fee_cat", "consultant", "adv_board", "obsvr", "fund_size_m", "closed"))
  check_names(con = conn, tbl_pm_fund_info_benchmark_info, c("pm_fund_info_id", "benchmark_info_id", "benchmark_type_id"))
  check_names(con = conn, tbl_pm_fund_nav_daily, c("pm_fund_info_id", "effective_date", "nav"))
  check_names(con = conn, tbl_pm_fund_portfolio, c("pm_fund_portfolio_id", "pm_fund_portfolio"))
  check_names(con = conn, tbl_pm_fund_sector, c("pm_fund_sector_id", "pm_fund_sector"))
  check_names(con = conn, tbl_pm_fund_sponsor, c("pm_fund_sponsor_id", "pm_fund_sponsor"))
  check_names(con = conn, tbl_ssbt_composite_book_of_record_daily, c("ssbt_composite_info_id", "effective_date", "beginning_market_value", "ending_market_value", "net_cash_flow", "daily_return"))
  check_names(con = conn, tbl_ssbt_composite_book_of_record_monthly, c("ssbt_composite_info_id", "effective_date", "beginning_market_value", "ending_market_value", "net_cash_flow", "monthly_return", "fiscal_ytd_return"))
  check_names(con = conn, tbl_ssbt_composite_info, c("ssbt_composite_info_id", "ssbt_composite_id", "ssbt_composite_description", "ssbt_composite_short_description", "benchmark_info_id"))
  check_names(con = conn, tbl_ssbt_composite_info_account_info, c("ssbt_composite_info_id", "account_info_id"))
  check_names(con = conn, tbl_ssbt_composite_info_benchmark_info, c("ssbt_composite_info_id", "benchmark_info_id", "benchmark_type_id"))
  check_names(con = conn, tbl_view_all_pm_fund_info, c("pm_fund_info_id", "pm_fund_id", "pm_fund_description", "pm_fund_common_name", "vintage", "commit", "unfunded", "legacy", "specialist", "invest_end", "term_end", "extension", "ext_time", "ext_used", "fee_cat", "consultant", "adv_board", "obsvr", "fund_size_m", "closed", "pm_fund_category", "pm_fund_category_description", "pm_fund_portfolio", "pm_fund_sponsor", "pm_fund_city", "pm_fund_sector"))
})
