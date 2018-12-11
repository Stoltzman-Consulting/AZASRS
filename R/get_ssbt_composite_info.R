
#' Query ssbt_composite_info table from database
#'
#' @param ... are for filtering the tables, can take values: category, portfolio, asset_class, sub_portfolio
#' @return Returns a tibble of all of the data
#' @examples
#' get_ssbt_composite_info(asset_class == 'Equities', category == 'Large')
#' @export
get_ssbt_composite_info = function(..., con = AZASRS_DATABASE_CONNECTION()){

  args = rlang::enexprs(...)
  tbl_name = 'ssbt_composite_info'


  usr_tbl = dplyr::tbl(con, tbl_name)
  dat = usr_tbl

  dat = usr_tbl %>%
    dplyr::left_join(tbl_category(con), by = c('category_id' = 'id')) %>%
    dplyr::left_join(tbl_portfolio(con), by = c('portfolio_id' = 'id')) %>%
    dplyr::left_join(tbl_asset_class(con), by = c('asset_class_id' = 'id')) %>%
    dplyr::left_join(tbl_sub_portfolio(con), by = c('sub_portfolio_id' = 'id'))

  dat = dat %>%
    dplyr::select(-asset_class_id, -portfolio_id, -sub_portfolio_id, -category_id)

  if(length(args) > 0){
    dat = dat %>%
      dplyr::filter(!!! args)
  }

  return(dat %>% tibble::as_tibble())
}
