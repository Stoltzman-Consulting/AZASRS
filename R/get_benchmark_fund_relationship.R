#' Get relationships between funds and benchmarks get_benchmark_fund_relationship
#'
#' @description Finds all data from benchmark_info_id matched with pm_fund_info_id
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @param bench_type is the type of benchmark. The default is 'PVT' which is private.
#' @param get_all_benchmark_types is a boolean that determines whether or not all bench types are returned
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or database object
#' @examples
#' get_benchmark_fund_relationship(return_tibble = TRUE)
#' # A tibble: 282 x 3
#' # pm_fund_info_id benchmark_info_id benchmark_type
#' # <int>                <int>               <chr>
#' #  1                    53                 PVT
#' #  2                    53                 PVT
#' #  3                    53                 PVT
#' # ... with 279 more rows
#' @export
###ADD THIS ONE###
get_benchmark_fund_relationship_raw <- function(){
  dat <- get_url_data("pm_fund_info")

  return(dat)
}



get_benchmark_fund_relationship <- function(bench_type = "PVT", get_all_benchmark_types = FALSE) {
  dat <- get_benchmark_fund_relationship_raw() %>%
    dplyr::left_join(get_benchmark_info(), by = "benchmark_type_id") %>%
    dplyr::select(pm_fund_info_id, benchmark_info_id, benchmark_type)

  if(!get_all_benchmark_types){
    dat = dat %>% dplyr::filter(benchmark_type == bench_type)
  }
    return(dat)
}


