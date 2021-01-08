#' Get relationships between funds and benchmarks get_benchmark_fund_relationship
#'
#' @description Finds all data from benchmark_info_id matched with pm_fund_info_id
#' @return Returns a tibble or database object
#' @examples
#' get_benchmark_fund_relationship()
#' # A tibble: 548 x 3
#' # pm_fund_info_id benchmark_info_id benchmark_type
#' # <int>                <int>               <chr>
#' #  1                    53                 PVT
#' #  2                    53                 PVT
#' #  3                    53                 PVT
#' # ... with 545 more rows
#' @export
###ADD THIS ONE###
get_benchmark_fund_relationship_raw <- function(){

  dat <- get_url_data("pm_fund_info_benchmark_info")

  return(dat)
}

#' Get relationships between funds and benchmarks get_benchmark_fund_relationship
#'
#' @description Finds all data from benchmark_info_id matched with pm_fund_info_id
#' @param con default TRUE
#' @param bench_type is the type of benchmark. The default is 'PVT' which is private.
#' @param get_all_benchmark_types is a boolean that determines whether or not all bench types are returned
#' @param return_tibble default FALSE, is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or database object
#' @examples
#' get_benchmark_fund_relationship()
#' # A tibble: 548 x 3
#' # pm_fund_info_id benchmark_info_id benchmark_type
#' # <int>                <int>               <chr>
#' #  1                    53                 PVT
#' #  2                    53                 PVT
#' #  3                    53                 PVT
#' # ... with 545 more rows
#' @export

get_benchmark_fund_relationship <- function(con = TRUE, bench_type = "PVT", get_all_benchmark_types = FALSE,
                                            return_tible = FALSE) {

  dat <- get_benchmark_fund_relationship_raw()

  if(!get_all_benchmark_types){
    dat = dat %>% dplyr::filter(benchmark_type == bench_type)
  }
    return(dat)

}


