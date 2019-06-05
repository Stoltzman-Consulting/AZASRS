
#' Query pm_cash_flow_daily table from database
#' @param ... are for filtering the tables, can take values: category, portfolio, asset_class, sub_portfolio, previous_saa, sponsor, saa_benchmark, imp_benchmark, ticker, city
#' @return Returns a tibble of all of the data
#' @examples
#' get_pm_cash_flow_daily(legacy == 'K')
#' @export
get_pm_cash_flow_daily = function(..., con = AZASRS_DATABASE_CONNECTION()){

  args = rlang::enexprs(...)

  dat = tbl_pm_fund_cash_flow_daily(con)

  pmfi = pm_fund_info(con = con, return_tibble=FALSE)

  dat = dat %>%
    dplyr::left_join(pmfi, by = c('pm_fund_info_id' = 'pm_fund_info_id')) %>%
    dplyr::mutate(contributions = ifelse(cash_flow < 0, cash_flow, 0),
                  distributions = ifelse(cash_flow > 0, cash_flow, 0))
  if(length(args) > 0){
    dat = dat %>%
      dplyr::filter(!!! args)
  }

  dat = dat %>% tibble::as_tibble() %>%
    dplyr::mutate(effective_date = as.Date(effective_date, format = '%Y-%m-%d')) %>%
    dplyr::filter(effective_date > '1900-01-01')

  return(dat)
}
