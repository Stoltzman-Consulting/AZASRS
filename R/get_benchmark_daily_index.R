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
get_benchmark_daily_index = function(..., con = AZASRS_DATABASE_CONNECTION(), bench_type = 'SAA', return_tibble = TRUE){
  args = rlang::enexprs(...)
  dat = tbl_benchmark_daily_index(con) %>%
    dplyr::left_join(tbl_pm_fund_info_benchmark_info(con), by = 'benchmark_info_id') %>%
    dplyr::left_join(tbl_benchmark_type_info(con), by = 'benchmark_type_info_id') %>%
    dplyr::left_join(tbl_pm_fund_info(con), by = 'pm_fund_info_id') %>%
    dplyr::filter(benchmark_type == bench_type)
  if(length(args) > 0){
    dat = dat %>%
      dplyr::filter(!!! args)
  }
  dat = dat %>% tibble::as_tibble() %>%
    dplyr::mutate(effective_date = as.Date(effective_date, format = '%Y-%m-%d')) %>%
    dplyr::filter(effective_date > '1900-01-01')
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}
