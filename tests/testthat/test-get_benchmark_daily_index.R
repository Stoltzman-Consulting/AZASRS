##Check:
##Column names are the same
##Column datatypes are the same
##there are at least x amount of rows


###testing raw data
test_that("get_benchmark_daily_index_raw() matches test data", {

  ##testing raw data##
  benchmark_daily_index_RAW <- get_benchmark_daily_index_raw()
  expected_names <- c("benchmark_id", "effective_date", "index_value")

  expect_equal(colnames(benchmark_daily_index_RAW), expected_names)

  expect_equal(as.character(lapply(benchmark_daily_index_RAW, class)), c("character", "Date", "numeric"  ))


  expect_true(dplyr::count(benchmark_daily_index_RAW) %>% dplyr::pull(n) > 40000)


  ###checking function
  benchmark_daily_index <- get_benchmark_daily_index()

  expected_names <- c( "benchmark_info_id", "benchmark_id", "effective_date", "index_value" )

  expect_equal(colnames(benchmark_daily_index), expected_names)

  expect_equal(as.character(lapply(benchmark_daily_index, class)), c("numeric", "character", "Date", "numeric" ))


  expect_true(dplyr::count(benchmark_daily_index) %>% dplyr::pull(n) > 40000)


})





test_that("get_benchmark_daily_index() matches test data", {
  benchmark_daily_index <- get_benchmark_daily_index()
  expected_names <- c("benchmark_info_id", "effective_date", "index_value", "benchmark_id")

  expect_equal(colnames(benchmark_daily_index), expected_names)


  expect_true(dplyr::count(benchmark_daily_index) %>% dplyr::pull(n) > 6963)
})

#Check:
#when all_bench_types = TRUE check number of rows is > bench_type = "PVT"

test_that("get_benchmark_daily_index() all bench types greater than just PVT", {
  ALL_bench_daily_index <- get_benchmark_daily_index(all_bench_types = TRUE)
  PVT_daily_index <- get_benchmark_daily_index(benchmark_type = "PVT")

  n_all <- dplyr::count(ALL_bench_daily_index) %>% dplyr::pull(n)
  n_pvt <- dplyr::count(PVT_daily_index) %>% dplyr::pull(n)

  expect_true(n_all > n_pvt)
})
