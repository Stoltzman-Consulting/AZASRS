
#' Query major info tables from database
#'
#' @param tbl_name is the name of the major info table (choose one): pm_fund_info, ssbt_composite_info, account_info
#' @param ... are for filtering the tables, can take values: category, portfolio, asset_class, sub_portfolio, previous_saa, sponsor, saa_benchmark, imp_benchmark, ticker, city
#' @return Returns a tibble of all of the data
#' @examples
#' get_tbl_info('pm_fund_info', sub_portfolio = 'Domestic')
#' @export
get_tbl_info = function(tbl_name, ..., con = AZASRS_DATABASE_CONNECTION()){

  if(!(tbl_name %in% c("pm_fund_info", "ssbt_composite_info", "account_info"))){
    return("Table name must be one the following: 'pm_fund_info', 'ssbt_composite_info', 'account_info'")
  }

  params = list(...)
  param_names = names(params)

  usr_tbl = dplyr::tbl(con, tbl_name)
  dat = usr_tbl

  dat = usr_tbl %>%
    dplyr::left_join(tbl_category(con), by = c('category_id' = 'id')) %>%
    dplyr::left_join(tbl_portfolio(con), by = c('portfolio_id' = 'id')) %>%
    dplyr::left_join(tbl_asset_class(con), by = c('asset_class_id' = 'id')) %>%
    dplyr::left_join(tbl_sub_portfolio(con), by = c('sub_portfolio_id' = 'id'))

  if(tbl_name == "account_info"){
    dat = dat %>%
      dplyr::left_join(tbl_previous_saa(con), by = c('previous_saa_id' = 'id')) %>%
      dplyr::left_join(tbl_sponsor(con), by = c('sponsor_id' = 'id')) %>%
      dplyr::left_join(tbl_saa_benchmark(con), by = c('saa_benchmark_id' = 'id')) %>%
      dplyr::left_join(tbl_imp_benchmark(con), by = c('imp_benchmark_id' = 'id')) %>%
      dplyr::left_join(tbl_ticker(con), by = c('ticker_id' = 'id')) %>%
      dplyr::left_join(tbl_city(con), by = c('city_id' = 'id'))
  }

  if(len(param_names) > 0){
    for(param in param_names){
      dat = dat %>%
        dplyr::filter(quote(param) == params[params[param]])
    }
  }

  dat = dat %>%
    dplyr::select(-benchmark_info_id, -asset_class_id, -portfolio_id, -sub_portfolio_id,
                  -category_id, -previous_saa_id, -sponsor_id, -saa_benchmark_id, -imp_benchmark_id, -ticker_id, -city_id)

  return(dat %>% tibble::as_tibble())
}
