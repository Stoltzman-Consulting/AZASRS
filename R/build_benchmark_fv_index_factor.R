#' Build a tibble of benchmark index future value factor
#'
#' @param ... grouping variables (pm_fund_id, pm_fund_portfolio, etc.)
#' @param benchmark_daily_index is data from get_benchmark_daily_index() and can be previously loaded and filtered
#' @param bench_type is string that represents one of the types (PVT, etc.)
#' @param end_date is the last quarter end, use for custom time ranges
#' @return Returns a tibble with grouping variables and all of their respective metrics
#' @export
build_benchmark_fv_index_factor = function(con = AZASRS_DATABASE_CONNECTION(),
                                           start_date = '2004-09-30',
                                           end_date = get_value_date(con = con),
                                           benchmark_daily_index = get_benchmark_daily_index(con = con,
                                                                                       all_bench_types = TRUE,
                                                                                       return_tibble = FALSE)){

  # Filter data and require tibble to allow for distinct + keep_all
  tmp = benchmark_daily_index %>%
    dplyr::filter(effective_date >= start_date,
                  effective_date <= end_date) %>%
    dplyr::as_tibble() %>%
    dplyr::distinct(benchmark_info_id, effective_date, .keep_all = TRUE)

  # Create a "future value" based off of index
  bench_daily_filtered = tmp %>%
    dplyr::group_by(benchmark_info_id) %>%
    dplyr::arrange(effective_date) %>%
    dplyr::mutate(index_fv = dplyr::last(index_value) / index_value) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(benchmark_info_id, effective_date)

  return(bench_daily_filtered)

}
