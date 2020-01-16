
#' Query pm_nav_daily table from database
#' @param ... are for filtering the tables, can take values: category, portfolio, asset_class, sub_portfolio, previous_saa, sponsor, saa_benchmark, imp_benchmark, ticker, city
#' @return Returns a tibble of all of the data
#' @examples
#' get_pm_nav_daily(legacy == 'K')
#' @export
get_pm_nav_daily = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble = TRUE){

  dat = tbl_pm_fund_nav_daily(con = con) %>%
    dplyr::left_join(tbl_view_all_pm_fund_info(con = con), by = 'pm_fund_info_id')

  if(return_tibble){
    return(dat %>% tibble::as_tibble())
  } else{
    return(dat)
  }

}
