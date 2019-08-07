#' Get total fund value
#'
#' @description Finds the most recent fund value for TOTAL FUND. Can be filtered to select by individual date.
#' @return <numeric> from ssbt_composite_book_of_record_daily by date selected (default = valdate)
#' @examples
#' get_total_fund_value()
#' # 38015261515
#'
#' get_total_fund_value(at_date = '2018-06-30')
#' # 39656200393
#' @export
get_total_fund_value = function(at_date = get_value_date(), con = AZASRS_DATABASE_CONNECTION()){
  dat = tbl_ssbt_composite_book_of_record_daily(con) %>%
    dplyr::filter(effective_date == at_date) %>%
    dplyr::left_join(tbl_ssbt_composite_info(con), by = 'ssbt_composite_info_id') %>%
    dplyr::filter(ssbt_composite_id == 'ASRSA001') %>%
    dplyr::select(ending_market_value) %>%
    dplyr::pull()
  return(dat)}
