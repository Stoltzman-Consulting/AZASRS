
#' Query major info tables from database
#'
#' @param tbl_name is the name of the major info table (choose one): pm_fund_info, ssbt_composite_info, account_info
#' @param ... are for filtering the tables, can take values: category, portfolio, asset_class, sub_portfolio
#' @return Returns a tibble of all of the data
#' @examples
#' get_tbl_info('pm_fund_info', sub_portfolio = 'Equity')
#' @export
get_tbl_info = function(tbl_name, ..., con = AZASRS_DATABASE_CONNECTION()){

  if(!(tbl_name %in% c("pm_fund_info", "ssbt_composite_info", "account_info"))){
    return("Table name must be one the following: 'pm_fund_info', 'ssbt_composite_info', 'account_info'")
  }

  params = list(...)
  param_names = names(params)

  # Always joining to info tables
  # ca = dplyr::tbl(con, "category")
  # po = dplyr::tbl(con, "portfolio")
  # a_c = dplyr::tbl(con, "asset_class")
  # s_p = dplyr::tbl(con, "sub_portfolio")

  usr_tbl = dplyr::tbl(con, tbl_name)

  dat = usr_tbl %>%
    dplyr::left_join(tbl_category(con), by = c('category_id' = 'id')) %>% dplyr::mutate(name.category = name) %>%
    dplyr::left_join(tbl_portfolio(con), by = c('portfolio_id' = 'id')) %>% dplyr::mutate(name.portfolio = name.y) %>%
    dplyr::left_join(tbl_asset_class(con), by = c('asset_class_id' = 'id')) %>% dplyr::mutate(name.asset_class = name) %>%
    dplyr::left_join(tbl_sub_portfolio(con), by = c('sub_portfolio_id' = 'id')) %>%dplyr:: mutate(name.sub_portfolio = name.y.y)

  if("category" %in% param_names){ dat = dat %>% dplyr::filter(name.category == params$category) }
  if("portfolio" %in% param_names){ dat = dat %>% dplyr::filter(name.portfolio == params$portfolio) }
  if("asset_class" %in% param_names){ dat = dat %>% dplyr::filter(name.asset_class == params$asset_class) }
  if("sub_portfolio" %in% param_names){ dat = dat %>% dplyr::filter(name.sub_portfolio == params$sub_portfolio) }

  dat = dat %>%
    dplyr::select(ssbt_id, description, name.category, name.portfolio, name.asset_class, name.sub_portfolio)

  return(dat %>% tibble::as_tibble())
}
