#' BUILD CASH FLOW TIBBLE firt for asrsMethods::irr.z
#'
#' @description NAV is setup as negative at first and positive at the end with all cash flows in between where contributions are negative and distributions are positive.
#' @param ... is the grouping set of variable(s) requested
#' @param start_date is the beginning date you would like the IRR to be calculated from
#' Should be in string format: 'yyyy-mm-dd'
#' @param end_date is the last date you would like the IRR to be calculated to
#' Should be in string format: 'yyyy-mm-dd'
#' @export
build_modified_cash_flow = function(..., con = AZASRS_DATABASE_CONNECTION(), start_date = '2017-12-31', end_date = get_value_date(), return_tibble = FALSE){

  nav_daily = get_pm_nav_daily(con = con, return_tibble = FALSE) %>%
    dplyr::filter(effective_date == start_date | effective_date == end_date) %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(cash_flow = sum(nav, na.rm = TRUE)) %>%
    dplyr::mutate(cash_flow = dplyr::if_else(effective_date == min(effective_date, na.rm = TRUE), -1*cash_flow, cash_flow))

  cf_daily = get_pm_cash_flow_daily(con = con, return_tibble = FALSE) %>%
    dplyr::filter(effective_date > start_date & effective_date < end_date) %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(cash_flow = sum(cash_flow, na.rm = TRUE))

  dat = dplyr::union(nav_daily, cf_daily) %>%
    dplyr::arrange(..., effective_date)

  if(return_tibble){
    return(dat %>% tibble::as_tibble())
  } else{
    return(dat)
  }

}
