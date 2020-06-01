#' Get all benchmark_daily_index
#'
#' @description Gets benchmark index data by day and only allows one bench_type (i.e. SAA). Joins data from pm_fund_info, benchmark_info and filters by benchmark type. Caution: returns A LOT of data, filter for better performance.
#' @param ... filtering expressions. i.e. effective_date >= '2018-01-01'
#' @return Returns a tibble with all benchmark index data but can be filtered.
#' @examples
#' get_benchmark_daily_index(effective_date >= '2018-01-01')
#' # A tibble: 134,953 x 28
#' # benchmark_info_id effective_date index_value pm_fund_info_id benchmark_type_info_id benchmark_type pm_fund_id
#' # <int>                <date>               <dbl>           <int>                  <int> <chr>          <chr>
#' #  64                 2018-01-01           3666.              38                      1 SAA            Fund1
#' #  64                 2018-01-01           3666.              39                      1 SAA            Fund2
#' #  64                 2018-01-01           3666.              40                      1 SAA            Fund3
#' # ... with 134,943 more rows, and 21 more variables: pm_fund_description <chr>, pm_fund_portfolio_id <int>,
#' #   pm_fund_category_id <int>, pm_fund_sponsor_id <int>, pm_fund_city_id <int>, pm_fund_sector_id <int>,
#' #   vintage <int>, commit <dbl>, unfunded <dbl>, legacy <chr>, specialist <chr>, invest_end <chr>, term_end <chr>,
#' #   extension <dbl>, ext_time <dbl>, ext_used <dbl>, fee_cat <chr>, consultant <chr>, adv_board <int>, obsvr <int>,
#' #   fund_size_m <dbl>
#' @export
get_benchmark_daily_index = function(con = AZASRS_DATABASE_CONNECTION(),
                                     bench_type = 'PVT',
                                     all_bench_types = FALSE,
                                     return_tibble = FALSE){

  if(all_bench_types){
    pmfi_bmi = get_benchmark_fund_relationship(con) %>%
      dplyr::distinct(benchmark_info_id)
  } else{
    pmfi_bmi = get_benchmark_fund_relationship(con) %>%
      dplyr::filter(benchmark_type == bench_type) %>%
      dplyr::distinct(benchmark_info_id)
  }

  dat = pmfi_bmi %>%
    dplyr::left_join(tbl_benchmark_daily_index(con), by = 'benchmark_info_id') %>%
    dplyr::left_join(tbl_benchmark_info(con) %>% dplyr::select(benchmark_info_id, benchmark_id), by = 'benchmark_info_id')

  if(return_tibble){
    return(dat %>% tibble::as_tibble()  %>%
             dplyr::mutate(effective_date = lubridate::as_date(effective_date)))
  } else{
    return(dat)
  }

}
