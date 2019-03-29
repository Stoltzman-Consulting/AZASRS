
#' Query pm_cash_flow_daily table from database
#' @param ... are for filtering the tables, can take values: category, portfolio, asset_class, sub_portfolio, previous_saa, sponsor, saa_benchmark, imp_benchmark, ticker, city
#' @return Returns a tibble of all of the data
#' @examples
#' get_pm_cash_flow_daily(legacy == 'K')
#' @export
get_pm_cash_flow_daily = function(..., con = AZASRS_DATABASE_CONNECTION()){

  args = rlang::enexprs(...)

  dat = tbl_pm_cash_flow_daily(con) %>%
    left_join(tbl_pm_fund_info(con), by = 'pm_fund_info_id') %>%
    as_tibble() %>%
    select(effective_date, pm_fund_id, cash_flow)

  if(length(args) > 0){
    dat = dat %>%
      dplyr::filter(!!! args)
  }
  return(dat %>% tibble::as_tibble())
}
