
#' Query ssbt_composite_info table from database
#'
#' @param ... are for filtering the tables, can take values: category, portfolio, asset_class, sub_portfolio
#' @return Returns a tibble of all of the data
#' @examples
#' get_ssbt_composite_info(asset_class == 'Equities', category == 'Large')
#' @export
get_ssbt_composite_info = function(..., con = AZASRS_DATABASE_CONNECTION()){

  args = rlang::enexprs(...)

  dat = ssbt_composite_info(con)

  if(length(args) > 0){
    dat = dat %>%
      dplyr::filter(!!! args)
  }

  return(dat %>% tibble::as_tibble())
}
