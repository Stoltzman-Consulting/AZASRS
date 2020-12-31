#' Get all pm_nav_daily RAW
#'
#' @description Gets pm_nav_daily table from database
#' @return Returns a tibble or database object
#' @examples
#' get_pm_nav_daily_raw()
#' # A tibble: 8,324 x 28
#' # pm_fund_info_id effective_date    nav     pm_fund_id    pm_fund_descrip…  pm_fund_common_… vintage commit unfunded
#' # <int>                <date>        <dbl>       <chr>     <chr>               <chr>              <int>  <int>    <int>
#' #  1                 2019-06-30      8.60e6      Hgh19     AP Mezzanine Pa…    HPS Mezz 2019      2019   6.00e8   3.95e8
#' #  1                 2019-09-30      8.51e7      Hgh19     AP Mezzanine Pa…    HPS Mezz 2019      2019   6.00e8   3.95e8
#' #  1                 2019-12-31      1.03e8      Hgh19     AP Mezzanine Pa…    HPS Mezz 2019      2019   6.00e8   3.95e8
#'
#' # … with 8,321 more rows, and 19 more variables: legacy <chr>, specialist <chr>, invest_end <date>,
#' #   term_end <date>, extension <dbl>, ext_time <dbl>, ext_used <dbl>, fee_cat <chr>, consultant <chr>,
#' #   adv_board <lgl>, obsvr <lgl>, fund_size_m <dbl>, closed <chr>, pm_fund_category <chr>,
#' #   pm_fund_category_description <chr>, pm_fund_portfolio <chr>, pm_fund_sponsor <chr>, pm_fund_city <chr>,
#' #   pm_fund_sector <chr>
#' @export
get_pm_nav_daily_raw <- function() {
  dat <- get_url_data("pm_fund_nav_quarterly")

  return(dat)

}

#' Get all pm_nav_daily
#'
#' @description Gets pm_nav_daily table from database
#' @return Returns a tibble or database object
#' @examples
#' get_pm_nav_daily()
#' # A tibble: 8,324 x 28
#' # pm_fund_info_id effective_date    nav     pm_fund_id    pm_fund_descrip…  pm_fund_common_… vintage commit unfunded
#' # <int>                <date>        <dbl>       <chr>     <chr>               <chr>              <int>  <int>    <int>
#' #  1                 2019-06-30      8.60e6      Hgh19     AP Mezzanine Pa…    HPS Mezz 2019      2019   6.00e8   3.95e8
#' #  1                 2019-09-30      8.51e7      Hgh19     AP Mezzanine Pa…    HPS Mezz 2019      2019   6.00e8   3.95e8
#' #  1                 2019-12-31      1.03e8      Hgh19     AP Mezzanine Pa…    HPS Mezz 2019      2019   6.00e8   3.95e8
#'
#' # … with 8,321 more rows, and 19 more variables: legacy <chr>, specialist <chr>, invest_end <date>,
#' #   term_end <date>, extension <dbl>, ext_time <dbl>, ext_used <dbl>, fee_cat <chr>, consultant <chr>,
#' #   adv_board <lgl>, obsvr <lgl>, fund_size_m <dbl>, closed <chr>, pm_fund_category <chr>,
#' #   pm_fund_category_description <chr>, pm_fund_portfolio <chr>, pm_fund_sponsor <chr>, pm_fund_city <chr>,
#' #   pm_fund_sector <chr>
#' @export
get_pm_nav_daily <- function() {

  dat <- get_pm_nav_daily_raw()

  return(dat)

}


