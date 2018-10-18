#' @importFrom magrittr %>%
#' @import DBI
#' @import RSQLite
#' @import zoo


# Global variables
AZASRS_DATABASE_LOCATION = "P:\\IMD\\2018 Database Project\\asrs.db"
AZASRS_DATABASE_DRIVER = RSQLite::SQLite()
dbCon = dplyr::src_sqlite(AZASRS_DATABASE_LOCATION)


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
  ca = dplyr::tbl(con, "category")
  po = dplyr::tbl(con, "portfolio")
  a_c = dplyr::tbl(con, "asset_class")
  s_p = dplyr::tbl(con, "sub_portfolio")

  usr_tbl = dplyr::tbl(con, tbl_name)

  dat = usr_tbl %>%
    dplyr::left_join(ca, by = c('category_id' = 'id'), suffix = c('.category', '')) %>%
    dplyr::left_join(po, by = c('portfolio_id' = 'id'), suffix = c('.portfolio', '')) %>%
    dplyr::left_join(a_c, by = c('asset_class_id' = 'id'), suffix = c('.asset_class', '')) %>%
    dplyr::left_join(s_p, by = c('sub_portfolio_id' = 'id'), suffix = c('.sub_portfolio', ''))

  if("category" %in% param_names){ dat = dat %>% dplyr::filter(name.category == params$category) }
  if("portfolio" %in% param_names){ dat = dat %>% dplyr::filter(name.portfolio == params$portfolio) }
  if("asset_class" %in% param_names){ dat = dat %>% dplyr::filter(name.asset_class == params$asset_class) }
  if("sub_portfolio" %in% param_names){ dat = dat %>% dplyr::filter(name.sub_portfolio == params$sub_portfolio) }

  return(dat)
}



# All tables (for now)
# a_i = tbl(con, "account_info")
# s_c = tbl(con, "ssbt_composite")
# s_c_i = tbl(con, "ssbt_composite_info")
# p_f_i = tbl(con, "pm_fund_info")
# s_p = tbl(con, "sub_portfolio")
# a_c = tbl(con, "asset_class")
# ca = tbl(con, "category")
# po = tbl(con, "portfolio")
# b_i = tbl(con, "benchmark_info")
# b_m = tbl(con, "benchmark_monthly")
# b_d = tbl(con, "benchmark_daily")
# p_c_q = tbl(con, "pm_cashflow_quarterly")
# p_g_o_q = tbl(con, "pm_gp_official_quarterly")
# b_o_r_m = tbl(con, "book_of_record_monthly")
# b_o_r_d = tbl(con, "book_of_record_daily")
