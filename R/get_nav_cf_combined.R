library(AZASRS)
#library(lubridate)
#library(tidyverse)

nav = get_pm_nav_daily() %>% dplyr::filter(nav != 0)
cf = get_pm_cash_flow_daily() %>% dplyr::filter(cash_flow != 0)
start_date = '2019-09-30'
#value_date = get_value_date()
value_date = '2019-12-31'
next_qtr_date = calc_add_qtrs(value_date, 1)

get_nav_on_dates = function(.data, start_date, end_date){
  # retrieves NAV on both start and end (if they exist)
  # start_date: effective_date that starts period being analyzed
  # end_date: effective_date that ends period being analyzed
  .data %>%
    dplyr::filter(effective_date == start_date | effective_date == end_date) %>%
    dplyr::mutate(db_table = 'pm_fund_nav_daily')
}

identify_single_nav = function(.data){
  # removes any fund that does not contain a NAV at start and end
  .data %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(has_reported = dplyr::if_else(dplyr::n() == 2, TRUE, FALSE)) %>%
    dplyr::ungroup()
}

get_cf_between_dates = function(.data, start_date, end_date){
  # function is inclusive of end dates
  # start_date: effective_date that starts period being analyzed
  # end_date: effective_date that ends period being analyzed
  .data %>%
    dplyr::filter(effective_date >= start_date) %>%
    dplyr::filter(effective_date <= end_date) %>%
    dplyr::mutate(db_table = 'pm_fund_cash_flow_daily')
}

convert_start_date_nav_to_negative = function(.data){
  # The -1*nav at the start date stands in as a cash flow
  .data %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(nav = dplyr::if_else(effective_date == min(effective_date, na.rm = TRUE),
                                       -1*nav,
                                       nav)) %>%
    dplyr::ungroup()
}

merge_nav_and_cf = function(.nav_data, .cf_data){
  # Combines nav and cash flow creating an adjusted column to combine them
  # Adds a has_reported field to use as a filter later on
  tmp_nav = .nav_data %>% dplyr::mutate(adjusted_cash_flow = nav)

  # Find all reported funds via NAV
  tmp_nav_reported = .nav_data %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::summarize(has_reported = all(has_reported, na.rm = TRUE))

  # Join has / has not reported to NAV
  tmp_cf = .cf_data %>%
    dplyr::mutate(adjusted_cash_flow = cash_flow) %>%
    dplyr::left_join(tmp_nav_reported, by = 'pm_fund_id')

  dplyr::union_all(tmp_nav, tmp_cf) %>%
    dplyr::group_by(has_reported, pm_fund_portfolio, pm_fund_category_description, pm_fund_common_name, pm_fund_description,pm_fund_id, effective_date) %>%
    dplyr::summarize(adjusted_cash_flow = sum(adjusted_cash_flow, na.rm = TRUE),
                     nav = sum(nav, na.rm = TRUE),
                     cash_flow = sum(cash_flow, na.rm = TRUE)) %>% # accounts for cf and nav on same day
    dplyr::ungroup() %>%
    dplyr::arrange(pm_fund_id, effective_date)
}

calculate_grouped_irr = function(.data, has_reported = TRUE, pm_fund_info = get_pm_fund_info(), ...){
  if(has_reported){
    .data = .data %>% dplyr::filter(has_reported)
  } else {
    max_date = max(.data$effective_date)
    bolt_on = .data %>%
      dplyr::filter(!has_reported) %>%
      dplyr::group_by(pm_fund_id) %>%
      dplyr::summarize(adjusted_cash_flow = sum(-1*adjusted_cash_flow, na.rm = TRUE)) %>%
      dplyr::mutate(effective_date = max_date) %>%
      dplyr::left_join(pm_fund_info, by = 'pm_fund_id')
    .data = dplyr::union_all(.data, bolt_on)
    print(.data)
  }
  .data %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(adjusted_cash_flow = sum(adjusted_cash_flow, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(irr = calc_irr(adjusted_cash_flow, effective_date))
}




##################usage
#valdate
nav_value_date = nav %>%
  get_nav_on_dates(start_date, value_date) %>%
  identify_single_nav() %>%
  convert_start_date_nav_to_negative()

cf_value_date = cf  %>%
  get_cf_between_dates(start_date, value_date)

nav_cf_value_date = nav_value_date %>%
  merge_nav_and_cf(cf_value_date)

# Only reported
nav_cf_value_date %>%
  calculate_grouped_irr(has_reported = TRUE, pm_fund_info = get_pm_fund_info(),
                        pm_fund_portfolio, pm_fund_category_description)

# Cash adjusted
nav_cf_value_date %>%
  calculate_grouped_irr(has_reported = FALSE, pm_fund_info = get_pm_fund_info(),
                        pm_fund_portfolio, pm_fund_category_description)

#
# # Next qtr
# nav_next_qtr = nav %>%
#   get_nav_on_dates(start_date, next_qtr_date) %>%
#   remove_single_nav()
#
# cf_next_qtr = cf  %>%
#   get_cf_between_dates(start_date, next_qtr_date)
#



# convert_nav_to_wide = function(.data){
#   tmp = .data %>%
#     dplyr::select(pm_fund_id, effective_date, nav) %>%
#     dplyr::arrange(pm_fund_id, effective_date) %>%
#     tidyr::pivot_wider(names_from = effective_date, values_from = nav)
#   colnames(tmp) = c('pm_fund_id', 'nav_start', 'nav_end')
#   tmp[, 2] = -1 * tmp[, 2] # acts as "cash flow" for irr purposes
#   return(tmp)
# }
