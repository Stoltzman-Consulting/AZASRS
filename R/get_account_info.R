
#' Query account info table from database
#'
#' @param ... are for filtering the tables, can take values: category, portfolio, asset_class, sub_portfolio, previous_saa, sponsor, saa_benchmark, imp_benchmark, ticker, city
#' @return Returns a tibble of all of the data
#' @examples
#' get_account_info(asset_class == 'Equities', category == 'Large')
#' @export
get_account_info = function(..., con = AZASRS_DATABASE_CONNECTION()){

  args = rlang::enexprs(...)

  dat = tbl_account_info(con)

  dat = dat %>%
    dplyr::left_join(tbl_category(con), by = c('category_id' = 'category_id')) %>%
    dplyr::left_join(tbl_portfolio(con), by = c('portfolio_id' = 'portfolio_id')) %>%
    dplyr::left_join(tbl_asset_class(con), by = c('asset_class_id' = 'asset_class_id')) %>%
    dplyr::left_join(tbl_sub_portfolio(con), by = c('sub_portfolio_id' = 'sub_portfolio_id')) %>%
    dplyr::left_join(tbl_previous_saa(con), by = c('previous_saa_id' = 'previous_saa_id')) %>%
    dplyr::left_join(tbl_sponsor(con), by = c('sponsor_id' = 'sponsor_id')) %>%
    dplyr::left_join(tbl_benchmark_info(con), by = c('saa_benchmark_id' = 'benchmark_info_id')) %>%
    dplyr::left_join(tbl_benchmark_info(con), by = c('imp_benchmark_id' = 'benchmark_info_id'))

  dat = dat %>%
    dplyr::select(-asset_class_id, -portfolio_id, -sub_portfolio_id, -saa_benchmark_id,
                  -benchmark_description.x, -benchmark_description.y, -imp_benchmark_id,
                  -category_id, -previous_saa_id, -sponsor_id, -previous_saa_id) %>%
    dplyr::rename(saa_benchmark_id = benchmark_id.x, imp_benchmark_id = benchmark_id.y)

  if(length(args) > 0){
    dat = dat %>%
      dplyr::filter(!!! args)
  }

  return(dat %>% tibble::as_tibble())
}
