# PACKAGE TESTING HELPER FUNCTIONS
check_database_tbl_matches = function(tbl_object, expected_names = c(), types = c(), greater_than_rows= n){
  a = tbl_object()
  testthat::expect_equal(
    class(a),
    c("tbl_Microsoft SQL Server", "tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")
  )
  b = tibble::as_tibble(a)
  testthat::expect_equal(colnames(b), expected_names)
  testthat::expect_equal(as.character(lapply(b, class)), types)
  testthat::expect_true(dplyr::count(b) %>% dplyr::pull(n) > greater_than_rows)
}
