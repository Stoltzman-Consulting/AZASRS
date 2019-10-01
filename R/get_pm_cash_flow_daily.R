#' Daily private market cash flows
#'
#' @description pm_cash_flow_daily table from database joined with metadata
#' @param ... is used for filtering. i.e. legacy == 'K'
#' @return Returns a tibble of cash flows and their metadata, can be filtered
#' @examples
#' get_pm_cash_flow_daily(legacy == 'K')
#' # A tibble: 3,457 x 32
#' # pm_fund_info_id effective_date  cash_flow pm_fund_id pm_fund_description  pm_fund_portfoli~ pm_fund_sponsor~
#' #  <int>             <date>          <dbl>    <chr>      <chr>                   <int>            <int>
#' #    226            2011-02-14     -12254945  Fund1      Fund Desc 1               3               15
#' #    69             2011-03-22     -15000000  Fund2      Fund Desc 2               2               22
#' #    176            2011-04-01      -3883655  Fund3      Fund Desc 3               2                6
#' # ... with 3,447 more rows, and 25 more variables: pm_fund_city_id <int>, pm_fund_sector_id <int>, vintage <int>,
#' #   commit <dbl>, unfunded <dbl>, legacy <chr>, specialist <chr>, invest_end <chr>, term_end <chr>, extension <dbl>,
#' #   ext_time <dbl>, ext_used <dbl>, fee_cat <chr>, consultant <chr>, adv_board <int>, obsvr <int>, fund_size_m <dbl>,
#' #   pm_fund_category <chr>, pm_fund_category_description <chr>, pm_fund_city <chr>, pm_fund_portfolio <chr>,
#' #   pm_fund_sector <chr>, pm_fund_sponsor <chr>, contributions <dbl>, distributions <dbl>
#' @export
get_pm_cash_flow_daily = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble = TRUE){

  dat = tbl_pm_fund_cash_flow_daily(con = con) %>%
    dplyr::mutate(contributions = ifelse(cash_flow < 0, cash_flow, 0),
                  distributions = ifelse(cash_flow > 0, cash_flow, 0)) %>%
    dplyr::left_join(tbl_view_all_pm_fund_info(con = con), by = 'pm_fund_info_id')

  if(return_tibble){
    return(dat %>% tibble::as_tibble())
  } else{
    return(dat)
    }
}
