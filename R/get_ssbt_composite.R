
#' Query ssbt_composite table from database
#'
#' @param ... are for filtering the tables, can take values: category, portfolio, asset_class, sub_portfolio, previous_saa, sponsor, saa_benchmark, imp_benchmark, ticker, city
#' @return Returns a tibble of all of the data
#' @examples
#' get_ssbt_composite(asset_class == 'Equities')
#' @export
get_ssbt_composite = function(..., con = AZASRS_DATABASE_CONNECTION()){

  args = rlang::enexprs(...)

  scb = dplyr::tbl(con, 'ssbt_composite') %>%
    dplyr::left_join(dplyr::tbl(con, 'book_of_record_daily'), by = c('ssbt_id' = 'ssbt_id'))

  dat = scb
  if(length(args) > 0){
    sci = get_ssbt_composite_info(...)
    dat = sci %>%
      dplyr::left_join(dat, by = c('ssbt_composite_id' = 'ssbt_composite_id'), copy = TRUE)
  }

  return(dat %>% tibble::as_tibble() %>% tidyr::drop_na(ssbt_id))

}
