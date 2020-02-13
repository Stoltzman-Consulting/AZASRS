#' Calculates IRR's given a date range
#'
#' @description Calculates cash flow list properly and adds NAV where required
#' @param ... is the grouping set of variable(s) requested
#' @param start_date is the beginning date you would like the IRR to be calculated from
#' Should be in string format: 'yyyy-mm-dd'
#' @param end_date is the last date you would like the IRR to be calculated to
#' Should be in string format: 'yyyy-mm-dd'
#' @examples
#' con = calc_grouped_irrs(pm_fund_portfolio, pm_fund_category, start_date = '2018-12-31')
#' @export
calc_grouped_irrs = function(...,
                             con = AZASRS_DATABASE_CONNECTION(),
                             start_date = '2017-12-31',
                             end_date = get_value_date(con = con),
                             itd = FALSE){

  dat = build_nav_cash_flow_combined(..., con = con,
                               start_date = start_date,
                               end_date = end_date,
                               itd = itd,
                               return_tibble = TRUE) %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(irr = calc_irr(cash_flow = nav_cf, dates = effective_date))

  return(dat)
  }
