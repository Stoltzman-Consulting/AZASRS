##Check:
##Column names are the same
##Column datatypes are the same
##there are at least x amount of rows

test_that("get_benchmark_fund_relationship() matches test data", {

  ###checking raw function
  benchmark_fund_relationship_RAW <- get_benchmark_fund_relationship_raw()
  expected_names <- c("pm_fund_info_id", "benchmark_info_id", "benchmark_type")

  expect_equal(colnames( benchmark_fund_relationship_RAW), expected_names)
  expect_equal(as.character(lapply( benchmark_fund_relationship_RAW, class)), c("numeric", "numeric",  "character"))
  expect_true(dplyr::count( benchmark_fund_relationship_RAW) %>% dplyr::pull(n) > 200)


  ###checking function
  benchmark_fund_relationship <- get_benchmark_fund_relationship()
  expected_names <- c("pm_fund_info_id", "benchmark_info_id", "benchmark_type")

  expect_equal(colnames(benchmark_fund_relationship), expected_names)
  expect_equal(as.character(lapply(benchmark_fund_relationship, class)), c("numeric", "numeric",  "character"))
  expect_true(dplyr::count(benchmark_fund_relationship) %>% dplyr::pull(n) > 200)


#Check:
#when get_all_benchmark_types = TRUE check number of rows is > bench_type = "PVT"
  ALL_fund_relationship <- get_benchmark_fund_relationship(get_all_benchmark_types = TRUE)

 PVT_fund_relationship <- get_benchmark_fund_relationship(bench_type = "PVT")

 n_all <- dplyr::count(ALL_fund_relationship) %>% dplyr::pull(n)

 n_pvt <- dplyr::count(PVT_fund_relationship) %>% dplyr::pull(n)

 expect_true(n_all > n_pvt)

})

