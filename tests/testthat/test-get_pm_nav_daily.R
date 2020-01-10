context("test-get_pm_nav_daily")

test_that("get_pm_nav_daily() matches test data", {
  pm_nav_daily = get_pm_nav_daily()
  pm_nav_daily_head = pm_nav_daily %>% head(3)
  pm_nav_daily_tail = pm_nav_daily %>% tail(3)

  expect_equal(pm_nav_daily_head, readRDS_test(pm_nav_daily_head))
  expect_equal(pm_nav_daily_tail, readRDS_test(pm_nav_daily_tail))
})
