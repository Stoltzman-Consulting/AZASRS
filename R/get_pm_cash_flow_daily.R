#' Daily private market cash flows RAW
#'
#' @description pm_cash_flow_daily table from database joined with metadata
#' @return Returns a tibble or database object.
#' @examples
#' get_pm_cash_flow_daily_raw()
#' @export
get_pm_cash_flow_daily_raw <- function() {

  dat <- get_url_data("pm_fund_cash_flow_daily")

  return(dat)

}

#' Daily private market cash flows
#'
#' @description pm_cash_flow_daily table from database joined with metadata
#' @param con default TRUE
#' @param return_tibble default TRUE, is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or database object.
#' @examples
#' get_pm_cash_flow_daily()
#' # A tibble: 11,586 x 30
#' # pm_fund_info_id effective_date cash_flow contributions distributions pm_fund_id pm_fund_descrip..
#' #  <int>            <date>           <dbl>     <dbl>         <dbl>   <chr>      <chr>
#' #    1            2019-06-17       -8.80e6   -8800000          0     Hgh19       AP Mezzanine Pa…
#' #    1            2019-08-09       -1.70e6   -1700000          0     Hgh19       AP Mezzanine Pa…
#' #    1            2019-08-29       -2.00e7   -20035179.      F 0     Hgh19       AP Mezzanine Pa…
#' # ... with  11,583 more rows, and 23 more variables: pm_fund_common_name <chr>, vintage <int>, commit <int>,
#' #   unfunded <int>, legacy <chr>, specialist <chr>, invest_end <date>, term_end <date>, extension <dbl>,
#' #   ext_time <dbl>, ext_used <dbl>, fee_cat <chr>, consultant <chr>, adv_board <lgl>, obsvr <lgl>,
#' #   fund_size_m <dbl>, closed <chr>, pm_fund_category <chr>, pm_fund_category_description <chr>,
#' #   pm_fund_portfolio <chr>, pm_fund_sponsor <chr>, pm_fund_city <chr>, pm_fund_sector <chr>
#' @export
get_pm_cash_flow_daily <- function(con = TRUE, return_tibble = TRUE) {

  dat <- get_pm_cash_flow_daily_raw() %>%
    dplyr::mutate(
      contributions = ifelse(cash_flow < 0, cash_flow, 0),
      distributions = ifelse(cash_flow > 0, cash_flow, 0)
    ) %>%
    dplyr::left_join(get_pm_fund_info(), by = "pm_fund_id")

    return(dat)
}


