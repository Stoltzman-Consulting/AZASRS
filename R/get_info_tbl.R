#' @importFrom magrittr %>%

#' Query major info tables from database
#'
#' Getting data from pm_fund_info, ssbt_composite_info, account_info with filtering possibilities
#' @param tbl_name is the name of the major info table (choose one): pm_fund_info, ssbt_composite_info, account_info
#' @param con is the database connection (defaults to P: drive)
#' @param ... are for filtering the tables, can take values: category, portfolio, asset_class, sub_portfolio
#' @examples
#' get_info_tbl('pm_fund_info', sub_portfolio='Equity')
#' @export
get_info_tbl = function(tbl_name, ..., con = dbCon){

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
    dplyr::left_join(ca(con), by = c('category_id' = 'id'), suffix = c('.category', '')) %>%
    dplyr::left_join(po(con), by = c('portfolio_id' = 'id'), suffix = c('.portfolio', '')) %>%
    dplyr::left_join(a_c(con), by = c('asset_class_id' = 'id'), suffix = c('.asset_class', '')) %>%
    dplyr::left_join(s_p(con), by = c('sub_portfolio_id' = 'id'), suffix = c('.sub_portfolio', ''))

  if("category" %in% param_names){ dat = dat %>% dplyr::filter(name.category == params$category) }
  if("portfolio" %in% param_names){ dat = dat %>% dplyr::filter(name.portfolio == params$portfolio) }
  if("asset_class" %in% param_names){ dat = dat %>% dplyr::filter(name.asset_class == params$asset_class) }
  if("sub_portfolio" %in% param_names){ dat = dat %>% dplyr::filter(name.sub_portfolio == params$sub_portfolio) }

  return(dat)
}
