context("test-benchmark_info")

##testing raw dataset
test_that("get_benchmark_info_raw() returns matching tibbles", {
  benchmark_info <- get_benchmark_info_raw()
   expected_names <- c("benchmark_id", "benchmark_description", "benchmark_symbol_name")

   expect_equal(colnames(benchmark_info), expected_names)
  expect_equal(as.character(lapply(benchmark_info, class)), c("character", "character", "character"))

  expect_true(dplyr::count(benchmark_info) %>% dplyr::pull(n) > 1)
})
