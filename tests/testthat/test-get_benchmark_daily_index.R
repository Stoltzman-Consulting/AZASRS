context("test-get_benchmark_daily_index")

test_that("get_benchmark_daily_index() returns matching tibble", {
  benchmark_daily_index = get_benchmark_daily_index()
  expect_equal(benchmark_daily_index, readRDS_test(benchmark_daily_index))
})

