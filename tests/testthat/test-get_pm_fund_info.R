context("test-get_pm_fund_info")

test_that("get_pm_fund_info() returns matching tibble", {
  pm_fund_info = get_pm_fund_info()
  pm_fund_info_head = pm_fund_info %>% head(3)

  expect_equal(pm_fund_info_head, readRDS_test(pm_fund_info_head))
  expect_true(dplyr::count(pm_fund_info) %>% dplyr::pull(n) > 277)
})
