#' Get all benchmark_daily_index RAW
#'
#' @description Gets benchmark index data by day
#' @return Returns a tibble or data.frame object.
#' @examples
#' get_benchmark_daily_index(return_tibble = TRUE)
#' @export
get_benchmark_daily_index_raw <- function() {

  dat <- get_url_data("benchmark_daily_index")

  return(dat)
}

#' Get all benchmark_daily_index
#'
#' @description Gets benchmark index data by day and only allows one bench_type (i.e. SAA). Joins data from pm_fund_info, benchmark_info and filters by benchmark type. Caution: returns A LOT of data, filter for better performance.
#' @param benchmark_type is the type of benchmark where default is 'PVT' which is private
#' @param all_bench_types is a boolean that determines whether or not all bench types are returned
#' @return Returns a tibble or database object.
#' @examples
#' get_benchmark_daily_index(return_tibble = TRUE)
#' # A tibble: 76,693 x 4
#' # benchmark_info_id effective_date index_value benchmark_id
#' # <int>                <date>               <dbl>           <chr>
#' #  53                 2004-01-02            1             LSTA+250
#' #  53                 2004-01-03            1.00          LSTA+250
#' #  53                 2004-01-04            1.00          LSTA+250
#' # ... with 76,690 more rows
#' @export
get_benchmark_daily_index <- function(benchmark_type = "PVT", all_bench_types = FALSE) {

  if (all_bench_types) {
    pmfi_bmi <- get_benchmark_fund_relationship(con, get_all_benchmark_types = all_bench_types) %>%
      dplyr::distinct(benchmark_info_id)
  } else {
    pmfi_bmi <- get_benchmark_fund_relationship(con) %>%
      dplyr::filter(benchmark_type == benchmark_type) %>%
      dplyr::distinct(benchmark_info_id)
  }

  dat <- pmfi_bmi %>%
    dplyr::left_join(get_benchmark_daily_index_raw(), by = "benchmark_info_id") %>%
    dplyr::left_join(get_benchmark_info() %>% dplyr::select(benchmark_info_id, benchmark_id), by = "benchmark_info_id")

  if (return_tibble) {
    return(dat %>% tibble::as_tibble() %>%
             dplyr::mutate(effective_date = lubridate::as_date(effective_date)))
  } else {
    return(dat)
  }
}

