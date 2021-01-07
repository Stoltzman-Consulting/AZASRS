context("test-get_pm_nav_daily")

###testing raw data
test_that("get_pm_nav_daily_raw() matches test data", {
  pm_nav_daily_RAW <- get_pm_nav_daily_raw()
  expected_names <- c("pm_fund_id","effective_date", "nav")

  expect_equal(colnames(pm_nav_daily_RAW), expected_names)
  expect_equal(as.character(lapply(pm_nav_daily_RAW, class)), c("character", "Date", "numeric"  ))
  expect_true(dplyr::count(pm_nav_daily_RAW) %>% dplyr::pull(n) > 8000)
})



test_that("get_pm_nav_daily() matches test data", {
  pm_nav_daily <- get_pm_nav_daily()
  expected_names <- c(
    "pm_fund_id", "effective_date",  "nav", "pm_fund_info_id",
    "pm_fund_description",          "pm_fund_common_name", "pm_fund_portfolio", "pm_fund_category",
    "pm_fund_category_description", "pm_fund_sponsor", "pm_fund_city", "pm_fund_sector",
    "vintage", "commit",   "unfunded","legacy",
    "specialist",  "invest_end",  "term_end", "extension",
    "ext_time", "ext_used", "fee_cat", "consultant",
    "adv_board",  "obsvr",  "fund_size_m", "closed"
  )

  expect_equal(colnames(pm_nav_daily), expected_names)
  expect_equal(as.character(lapply(pm_nav_daily, class)), c("character", "Date",     "numeric",   "numeric",   "character", "character", "character", "character", "character", "character",
                                                            "character", "character", "numeric",   "numeric",  "numeric",   "character", "character", "Date" ,     "Date",     "numeric",
                                                            "numeric",   "numeric",   "character", "character", "logical",   "logical",   "numeric",  "character"))
  expect_true(dplyr::count(pm_nav_daily) %>% dplyr::pull(n) > 8000)
})
