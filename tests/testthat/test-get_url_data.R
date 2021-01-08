context("test-get_url_data")

test_that("get_url_data() returns tibble", {
  url_data <- get_url_data("pm_fund_info")

  expect_equal(class(url_data), c("spec_tbl_df", "tbl_df", "tbl",  "data.frame" ))

  expect_true(dplyr::count(url_data) %>% dplyr::pull(n) > 1)
})
