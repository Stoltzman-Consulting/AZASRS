#' Get SSBT composite book of record monthly
#'
#' @description Gets composite book of record monthly
#' @param ... filtering expressions. i.e. effective_date >= '2018-01-01'
#' @return Returns a tibble with all composite data but can be filtered.
#' @examples
#' get_ssbt_composite_bor_monthly(effective_date >= "2018-01-01")
#' # A tibble: 4,160 x 10
#' # ssbt_composite_in~ effective_date   beginning_market~ ending_market_v~ net_cash_flow monthly_return fiscal_ytd_retu~
#' #   <int>               <chr>              <dbl>            <dbl>         <dbl>        <dbl>           <chr>
#' #    24               2018-12-31         1467349533.      1482968778.          0       0.0106          0
#' #    25               2018-12-31         1482974842.      1463381621.          0       0               0
#' # ... with 13 more rows, and 4 more variables: ssbt_composite_id <chr>, ssbt_composite_description <chr>,
#' #   ssbt_composite_short_description <chr>, benchmark_info_id <int>
#' @export
get_ssbt_composite_bor_monthly <- function(con = AZASRS_DATABASE_CONNECTION(), return_tibble = TRUE) {
  dat <- tbl_ssbt_composite_book_of_record_monthly(con) %>%
    dplyr::left_join(tbl_ssbt_composite_info(con), by = "ssbt_composite_info_id")

  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
