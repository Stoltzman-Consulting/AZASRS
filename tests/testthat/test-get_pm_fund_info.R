context("test-get_pm_fund_info")

test_that("get_pm_fund_info() returns matching tibble", {
  pm_fund_info <- get_pm_fund_info()
  expected_columns <- c(
    "pm_fund_id", "pm_fund_description",   "pm_fund_common_name",
    "pm_fund_portfolio", "pm_fund_category","pm_fund_category_description",
    "pm_fund_sponsor",   "pm_fund_city",
    "pm_fund_sector", "vintage" ,
    "commit",  "unfunded", "legacy",  "specialist",
    "invest_end", "term_end", "extension", "ext_time" ,
    "ext_used" , "fee_cat" ,"consultant", "adv_board",
    "obsvr", "fund_size_m" ,"closed"
  )

  expect_equal(colnames(pm_fund_info), expected_columns)
  expect_equal(as.character(lapply(pm_fund_info, class)), c("character", "character", "character", "character", "character", "character",
                                                            "character", "character", "character", "numeric",   "numeric" ,  "numeric",
                                                            "character", "character", "Date",  "Date", "numeric",   "numeric",
                                                            "numeric", "character", "character", "logical", "logical",   "numeric" ,
                                                            "character"))
  expect_true(dplyr::count(pm_fund_info) %>% dplyr::pull(n) > 1)
})
