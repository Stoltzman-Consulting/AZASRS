context("test-get_pm_fund_info")

test_that("get_pm_fund_info() returns matching tibble", {
  pm_fund_info = get_pm_fund_info()
  pm_fund_info_head = pm_fund_info %>% head(3)

  pm_fund_info_benchmarks = get_pm_fund_info(add_benchmark = TRUE) %>% head(3)

  expect_equal(pm_fund_info_head, readRDS_test(pm_fund_info_head))
  expect_equal(pm_fund_info_benchmarks, readRDS_test(pm_fund_info_benchmarks))
  expect_true(dplyr::count(pm_fund_info) %>% dplyr::pull(n) > 277)
})
