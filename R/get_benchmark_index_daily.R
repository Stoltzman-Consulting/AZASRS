
#' Query benchmark_index table from database
#' @param ... are for filtering the tables, can take values: category, portfolio, asset_class, sub_portfolio, previous_saa, sponsor, saa_benchmark, imp_benchmark, ticker, city
#' @return Returns a tibble of all of the data
#' @examples
#' get_benchmark_index_daily(effective_date >= '2018-01-01')
#' @export
get_benchmark_index_daily = function(..., con = AZASRS_DATABASE_CONNECTION(), bench_type = 'SAA'){

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

  return(dat)
}

