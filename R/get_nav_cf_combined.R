library(AZASRS)
#library(lubridate)
#library(tidyverse)

nav = get_pm_nav_daily() %>% dplyr::filter(nav != 0)
cf = get_pm_cash_flow_daily() %>% dplyr::filter(cash_flow != 0)
pm_fund_info = get_pm_fund_info()

# Internal funcs only
test_is_not_rollup <- function(...) {
  # Helper function for determing whether or not data is rolled up
  # A TRUE response means that it will not be rolled up
  any(c("pm_fund_id", "pm_fund_description", "pm_fund_common_name") %in% c(
    rlang::enquos(...) %>%
      purrr::map(rlang::quo_name) %>%
      unlist()
  ))
}


# Line up funcs
filter_nav_on_dates = function(.data, start_date, end_date, itd){
  # retrieves NAV on both start and end (if they exist)
  # start_date: effective_date that starts period being analyzed
  # end_date: effective_date that ends period being analyzed
  # itd: inception to date
  if(itd){
    # ITD does not require a start NAV (covered by cash flow)
    # ITD may require an ending nav (if it is not closed)
    .data %>%
      dplyr::filter(effective_date == end_date)
  } else{
    # Both are captured if not ITD, however, some may  not have reported yet
  .data %>%
    dplyr::filter(effective_date == start_date | effective_date == end_date)
    }
}


append_nav_has_reported <- function(.data, end_date){
  .data %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(has_reported = max(effective_date) == end_date) %>%
    dplyr::ungroup() %>%
    dplyr::select(has_reported, dplyr::everything())
}


convert_start_date_nav_to_negative = function(.data, start_date, itd){
  # The -1*nav at the start date stands in as a cash flow
  .data %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(nav = dplyr::if_else(effective_date == start_date,
                                       -1*nav,
                                       nav)) %>%
    dplyr::ungroup()
}


get_cf_between_dates = function(.data, start_date, end_date, itd){
  # function is inclusive of end dates
  # start_date: effective_date that starts period being analyzed
  # end_date: effective_date that ends period being analyzed
  # itd: inception to date
  if(itd){
    .data %>%
      dplyr::filter(effective_date <= end_date)
  } else{
  .data %>%
    dplyr::filter(effective_date >= start_date,
                  effective_date <= end_date)
  }
}


merge_nav_and_cf = function(.nav_data, .cf_data){
  # Combines nav and cash flow creating an adjusted column to combine them
  # Adds a has_reported field to use as a filter later on
  nav_cf_combined = dplyr::union_all(.nav_data %>% dplyr::mutate(cash_flow = 0),
                                     .cf_data %>% dplyr::mutate(nav = 0))
}


filter_dates = function(.data, start_date, end_date, itd, ...){

  # Filter out funds that are not active for the full start - end period.
  # Not applicable to aggregated funds, would filter out important data
  if (test_is_not_rollup(...)) {
    if (!itd) {
      .data <- .data %>%
        dplyr::group_by(pm_fund_id) %>%
        dplyr::filter(min(effective_date) <= start_date) %>%
        dplyr::filter(max(effective_date) >= end_date) %>%
        dplyr::ungroup()
    }
  }
  return(.data)
}


clean_nav_cf = function(.data, pm_fund_info){
  .data %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(adjusted_cash_flow = nav + cash_flow) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(pm_fund_id, effective_date) %>%
    dplyr::summarize(adjusted_cash_flow = sum(adjusted_cash_flow),
              nav = sum(nav),
              cash_flow = sum(cash_flow)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(pm_fund_info, by = 'pm_fund_id')
}


calculate_grouped_irr = function(.data, ...){
  .data %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(adjusted_cash_flow = sum(adjusted_cash_flow, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(irr = calc_irr(adjusted_cash_flow, effective_date))
}




##################usage

my_calcs = function(start_date, end_date, itd, pm_fund_info, ...){

  # itd failsafe
  if(itd){
    start_date = '2004-06-30'
  }

  nav1 = nav %>%
    filter_nav_on_dates(start_date = start_date, end_date = end_date, itd = itd) %>%
    append_nav_has_reported(end_date = end_date) %>%
    convert_start_date_nav_to_negative(start_date = start_date, itd = itd)

  cf1 = cf %>%
    get_cf_between_dates(start_date = start_date, end_date = end_date, itd = itd)

  clean_data = merge_nav_and_cf(nav1, cf1) %>%
    filter_dates(start_date = start_date, end_date = end_date, itd = itd, ...) %>%
    clean_nav_cf(pm_fund_info = pm_fund_info)

  clean_data %>%
    calculate_grouped_irr(...)
  }

# start_date = '2019-06-30'
# end_date = '2019-09-30'
# itd = FALSE
# d = my_calcs(start_date = start_date, end_date = end_date, itd = itd, pm_fund_info = pm_fund_info,
#          pm_fund_portfolio, pm_fund_category_description)

