
#' Query pm_fund_info table from database
#'
#' @param ... are for filtering the tables, can take values: category, portfolio, asset_class, sub_portfolio, previous_saa, sponsor, saa_benchmark, imp_benchmark, ticker, city
#' @return Returns a tibble of all of the data
#' @examples
#' get_pm_fund_info(asset_class == 'Equities', category == 'Large')
#' @export
get_pm_fund_info = function(..., con = AZASRS_DATABASE_CONNECTION()){

  args = rlang::enexprs(...)

  dat = tbl_pm_fund_info(con)

  dat = dat %>%
    dplyr::left_join(tbl_category(con), by = c('category_id' = 'category_id')) %>%
    dplyr::left_join(tbl_portfolio(con), by = c('portfolio_id' = 'portfolio_id')) %>%
    dplyr::left_join(tbl_sponsor(con), by = c('sponsor_id' = 'sponsor_id')) %>%
    dplyr::left_join(tbl_city(con), by = c('city_id' = 'city_id')) %>%
    dplyr::left_join(tbl_benchmark_info(con), by = c('benchmark_info_id' = 'benchmark_info_id'))

  dat = dat %>%
    dplyr::select(-category_id, -portfolio_id, -sponsor_id, -city_id, -benchmark_info_id)

  if(length(args) > 0){
    dat = dat %>%
      dplyr::filter(!!! args)
  }

  return(dat %>% tibble::as_tibble())
}
