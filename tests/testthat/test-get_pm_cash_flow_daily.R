context("test-get_pm_cash_flow_daily")

##testing raw dataset
test_that("get_pm_cash_flow_daily_raw() returns matching tibbles", {

  ###testing raw data###
  cf_RAW <- get_pm_cash_flow_daily_raw()
  expected_names <- c("pm_fund_id", "effective_date", "cash_flow")

  expect_equal(colnames(cf_RAW), expected_names)
  expect_equal(as.character(lapply(cf_RAW, class)), c("character", "Date", "numeric"))

  expect_true(dplyr::count(cf_RAW) %>% dplyr::pull(n) > 10000)
})



test_that("get_pm_cash_flow_daily() returns matching tibbles", {

  cf <- get_pm_cash_flow_daily()
  expected_names <- c(
    "pm_fund_id", "effective_date", "cash_flow",  "contributions",
    "distributions", "pm_fund_info_id","pm_fund_description", "pm_fund_common_name",
    "pm_fund_portfolio",   "pm_fund_category", "pm_fund_category_description", "pm_fund_sponsor",
    "pm_fund_city", "pm_fund_sector",   "vintage", "commit",
    "unfunded", "legacy", "specialist", "invest_end",
    "term_end", "extension", "ext_time", "ext_used",
    "fee_cat",  "consultant", "adv_board", "obsvr",
    "fund_size_m", "closed"
  )
  expect_equal(colnames(cf), expected_names)
  expect_equal(as.character(lapply(cf, class)), c("character", "Date","numeric", "numeric",  "numeric" ,  "numeric",   "character", "character", "character", "character",
                                                  "character", "character", "character", "character", "numeric",   "numeric",   "numeric",  "character", "character", "Date",
                                                  "Date", "numeric",   "numeric",   "numeric",   "character", "character", "logical",   "logical",  "numeric",   "character"))

  expect_true(dplyr::count(cf) %>% dplyr::pull(n) > 11000)
})
