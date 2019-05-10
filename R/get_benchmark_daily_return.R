
#' @export
get_benchmark_daily_return = function(){
  con = AZASRS_DATABASE_CONNECTION()
  dat = tbl_benchmark_daily_return(con) %>%
    dplyr::left_join(tbl_benchmark_info(con), by = 'benchmark_info_id') %>%
    tibble::as_tibble()
  return(dat)
}
