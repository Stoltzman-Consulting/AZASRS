#' Get all account_info
#'
#' @description A view to get all account info, can be filtered. By default, SQL --> SELECT * FROM all_account_info;
#' @param ... filtering expressions. i.e. account_asset_class == 'Cash'
#' @return Returns a tibble with all account_info metadata.
#' @examples
#' get_account_info(account_asset_class == 'Cash')
#' A tibble: 4 x 13
#' # account_info_id account_id quarter_lagged inception defunding exp_excess exp_te account_asset_cl~ account_category
#' # <int> <chr>               <int>     <dbl>     <dbl>      <dbl>  <dbl> <chr>             <chr>
#' # 1               1 Acc1                    1        NA        NA         NA     NA Cash              ""
#' # 2               2 Acc2                    1        NA        NA         NA     NA Cash              ""
#' # 3               3 Acc3                    1        NA        NA         NA     NA Cash              ""
#' # 4               4 Acc4                    1        NA        NA         NA     NA Cash              ""
#' # ... with 4 more variables: account_portfolio <chr>, account_sponsor <chr>, account_sub_portfolio <chr>,
#' #   account_sponsor_id <int>
#' @export
get_account_info = function(..., con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  args = rlang::enexprs(...)
  dat = dplyr::tbl(con, "all_account_info")
  if(length(args) > 0){
    dat = dat %>%
      dplyr::filter(!!! args)
  }
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}
