context("test-connect_to_all_tables")

test_that("I can retrieve data from all tbl_ functions", {
  con = AZASRS_DATABASE_CONNECTION()
  expect_true(length(dplyr::src_tbls(con)) > 1)

  db_tbl = tbl_account_asset_class(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 2)

  db_tbl = tbl_account_book_of_record_daily(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 6)

  db_tbl = tbl_account_book_of_record_monthly(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 7)

  db_tbl = tbl_account_category(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 2)

  db_tbl = tbl_account_info(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 12)

  db_tbl = tbl_account_info_benchmark_info(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 3)

  db_tbl = tbl_account_info_pm_fund_info(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 2)

  db_tbl = tbl_account_portfolio(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 2)

  db_tbl = tbl_account_sponsor(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 2)

  db_tbl = tbl_account_sub_portfolio(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 2)

  db_tbl = tbl_benchmark_daily_index(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 3)

  db_tbl = tbl_benchmark_daily_return(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 3)

  db_tbl = tbl_benchmark_info(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 4)

  db_tbl = tbl_benchmark_monthly_return(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 4)

  db_tbl = tbl_benchmark_type_info(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 2)

  db_tbl = tbl_constants(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 3)

  db_tbl = tbl_pm_fund_cash_flow_daily(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 3)

  db_tbl = tbl_pm_fund_category(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 3)

  db_tbl = tbl_pm_fund_city(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 2)

  db_tbl = tbl_pm_fund_info(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 23)

  db_tbl = tbl_pm_fund_info_benchmark_info(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 3)

  db_tbl = tbl_pm_fund_nav_daily(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 3)

  db_tbl = tbl_pm_fund_portfolio(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 2)

  db_tbl = tbl_pm_fund_sector(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 2)

  db_tbl = tbl_pm_fund_sponsor(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 2)

  db_tbl = tbl_ssbt_composite_book_of_record_daily(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 6)

  db_tbl = tbl_ssbt_composite_book_of_record_monthly(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 7)

  db_tbl = tbl_ssbt_composite_info(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 5)

  db_tbl = tbl_ssbt_composite_info_account_info(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 2)

  db_tbl = tbl_ssbt_composite_info_benchmark_info(con)
  expect_true(unique(class(db_tbl) == c("tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))
  expect_true(length(colnames(db_tbl)) == 3)

})
