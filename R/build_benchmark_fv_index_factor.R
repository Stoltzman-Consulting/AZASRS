#' Build a tibble of benchmark index future value factor
#'
#' @param ... grouping variables (pm_fund_id, pm_fund_portfolio, etc.)
#' @param benchmark_daily is data from get_benchmark_daily_index() and can be previously loaded and filtered
#' @param bench_type is string that represents one of the types (PVT, etc.)
#' @param value_date is the last quarter end, use for custom time ranges
#' @return Returns a tibble with grouping variables and all of their respective metrics
#' @export
build_benchmark_fv_index_factor = function(...,
                                           con = AZASRS_DATABASE_CONNECTION(),
                                           start_date = '2004-09-30',
                                           value_date = get_value_date(),
                                           bench_type = 'PVT',
                                           benchmark_daily = get_benchmark_daily_index(con = con, bench_type = bench_type, return_tibble = FALSE),
                                           return_tibble = FALSE){

  bmd = benchmark_daily %>%
    dplyr::filter(effective_date <= value_date) %>%
    dplyr::filter(effective_date >= start_date)

  bmd_end = bmd %>%
    dplyr::filter(effective_date == value_date) %>%
    dplyr::select(benchmark_info_id, index_value) %>%
    dplyr::distinct(benchmark_info_id, index_value) %>%
    dplyr::rename(last_index_value = index_value)

  dat = bmd %>%
    dplyr::left_join(bmd_end, by = 'benchmark_info_id') %>%
    dplyr::mutate(index_factor = last_index_value / index_value) %>%
    dplyr::select(benchmark_info_id, benchmark_id, effective_date, index_value, index_factor)

  if(return_tibble == TRUE){
    return(dat %>% tibble::as_tibble())
  }
  else{
    return(dat)
  }
}
