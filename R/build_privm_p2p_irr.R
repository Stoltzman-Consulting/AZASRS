
#' BUILD PRIVATE MARKET P2P IRR TIBBLE
#'
#' @description Creates a tibble of all Private Market IRRs found within the pm_fund_info table
#' @param start_date is the beginning date you would like the IRR to be calculated from
#' Should be in string format: 'yyyy-mm-dd'
#' @param end_date is the last date you would like the IRR to be calculated to
#' Should be in string format: 'yyyy-mm-dd'
#' @export
build_privm_p2p_irr = function(start_date = '2017-12-31', end_date = get_value_date()){

  pmfi = get_pm_fund_info()

  nav_daily = get_pm_nav_daily(effective_date == !!start_date | effective_date == !!end_date) %>%
    dplyr::select(pm_fund_id, effective_date, nav) %>%
    dplyr::mutate(cash_flow_mod = nav, cash_flow = 0)
  # removes NA for upcoming bind_rows

  cf_daily = get_pm_cash_flow_daily(effective_date >= !!start_date & effective_date <= !!end_date) %>%
    dplyr::select(pm_fund_id, effective_date, cash_flow) %>%
    dplyr::mutate(cash_flow_mod = cash_flow, nav = 0) # removes NA for upcoming bind_rows

  nav_cf = dplyr::bind_rows(nav_daily, cf_daily) %>%
    dplyr::group_by(pm_fund_id, effective_date) %>%
    dplyr::summarize(cash_flow_mod = sum(cash_flow_mod)) %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(cash_flow_mod = dplyr::if_else(effective_date == min(effective_date),
                                                 -1*abs(cash_flow_mod), cash_flow_mod)) %>% #creates negative first cash_flow
    dplyr::ungroup() %>%
    dplyr::group_by(pm_fund_id, effective_date) %>%
    dplyr::summarize(cash_flow_mod = sum(cash_flow_mod)) %>%
    dplyr::left_join(pmfi, by = 'pm_fund_id')

  grouped_irrs = function(small_group, nav_cf, grouping_type, TOTAL_FUND = FALSE){

    if(TOTAL_FUND){
      irr = nav_cf %>%
        dplyr::group_by(effective_date) %>%
        dplyr::summarize(cash_flow_mod = sum(cash_flow_mod)) %>%
        dplyr::summarize(irr = calc_irr(cash_flow_mod, effective_date)) %>%
        dplyr::mutate(grouping_type = grouping_type, name = 'TOTAL FUND')
      return(irr)
    }

    small_group = rlang::enquo(small_group)
    irr = nav_cf %>%
      dplyr::group_by(!!small_group, effective_date) %>%
      dplyr::summarize(cash_flow_mod = sum(cash_flow_mod)) %>%
      dplyr::group_by(!!small_group) %>%
      dplyr::summarize(irr = calc_irr(cash_flow_mod, effective_date)) %>%
      dplyr::mutate(grouping_type = grouping_type, name = !!small_group) %>%
      dplyr::select(grouping_type, name, irr)
    return(irr)
  }

  irr_portfolio = grouped_irrs(small_group = pm_fund_portfolio, grouping_type = 'pm_fund_portfolio',  nav_cf = nav_cf)
  irr_category = grouped_irrs(small_group = pm_fund_category, grouping_type = 'pm_fund_category', nav_cf = nav_cf)
  irr_category_description = grouped_irrs(small_group = pm_fund_category_description, grouping_type = 'pm_fund_category_description', nav_cf = nav_cf)
  irr_sponsor = grouped_irrs(small_group = pm_fund_sponsor, grouping_type = 'pm_fund_sponsor', nav_cf = nav_cf)
  irr_city = grouped_irrs(small_group = pm_fund_city, grouping_type = 'pm_fund_city', nav_cf = nav_cf)
  irr_sector = grouped_irrs(small_group = pm_fund_sector, grouping_type = 'pm_fund_sector', nav_cf = nav_cf)
  irr_fund_id = grouped_irrs(small_group = pm_fund_id, grouping_type = 'pm_fund_id', nav_cf = nav_cf)
  irr_fund_description = grouped_irrs(small_group = pm_fund_description, grouping_type = 'pm_fund_fund_description', nav_cf = nav_cf)
  irr_total_fund = grouped_irrs(small_group = pm_fund_description, grouping_type = 'TOTAL_FUND', nav_cf = nav_cf, TOTAL_FUND = TRUE)

  all_irr = dplyr::bind_rows(irr_fund_id, irr_fund_description,
                      irr_portfolio, irr_category, irr_category_description,
                      irr_sponsor, irr_city, irr_sector, irr_total_fund)

  return(all_irr)
}


