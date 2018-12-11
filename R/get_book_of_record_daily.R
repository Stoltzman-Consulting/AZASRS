
#' Query book_of_record table from database
#'
#' @param ... are for filtering the tables, can take values: category, portfolio, asset_class, sub_portfolio, previous_saa, sponsor, saa_benchmark, imp_benchmark, ticker, city
#' @return Returns a tibble of all of the data
#' @examples
#' get_book_of_record_daily(asset_class == 'Equities')
#' @export
get_book_of_record_daily = function(..., con = AZASRS_DATABASE_CONNECTION()){

  args = rlang::enexprs(...)

  tbl_name = 'book_of_record_daily'
  usr_tbl = dplyr::tbl(con, tbl_name)

  dat = usr_tbl
  if(length(args) > 0){
    ai = get_account_info(...)
    dat = ai %>%
      dplyr::left_join(dat, by = c('ssbt_id' = 'ssbt_id'), copy = TRUE)
  }

  return(dat %>% tibble::as_tibble() %>% tidyr::drop_na(ssbt_id))
}
