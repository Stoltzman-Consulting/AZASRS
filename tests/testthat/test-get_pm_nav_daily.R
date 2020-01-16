context("test-get_pm_nav_daily")

test_that("get_pm_nav_daily() matches test data", {
  pm_nav_daily = get_pm_nav_daily()
  pm_nav_daily_head = pm_nav_daily %>% arrange(pm_fund_info_id, effective_date) %>% head(3)
  expect_equal(pm_nav_daily_head, readRDS_test(pm_nav_daily_head))
  expect_true(count(pm_nav_daily) %>% dplyr::pull(n) > 7115)
})
