library(AZASRS)
#library(lubridate)
#library(tidyverse)



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



merge_nav_and_cf = function(.nav_data, .cf_data, end_date, cash_adjusted, pm_fund_info){
  # Combines nav and cash flow creating an adjusted column to combine them
  # Adds a has_reported field to use as a filter later on
  .nav_data = .nav_data %>% dplyr::mutate(cash_flow = 0)
  .cf_data = .cf_data %>% dplyr::mutate(nav = 0)

  if(!cash_adjusted){
    .nav_data = .nav_data %>%
      dplyr::filter(has_reported)

    funds_reported = .nav_data %>%
      dplyr::select(pm_fund_id) %>%
      dplyr::pull()

    .cf_data = .cf_data %>%
      dplyr::filter(pm_fund_id %in% funds_reported)

    return(dplyr::union_all(.nav_data, .cf_data))

  } else{
    # Find out which funds haven't reported
    funds_not_reported_data = .nav_data %>%
      dplyr::filter(!has_reported)

    # Pull only names not reported
    funds_not_reported_names = funds_not_reported_data %>%
      dplyr::select(pm_fund_id) %>%
      dplyr::pull()


    # Filter names to extract cash flows
    # Replace all dates with end date
    # Reverse sign to signal increase in NAV
    # Bind rows to previous NAV to adjust
    cf_as_nav = .cf_data %>%
      dplyr::filter(pm_fund_id %in% funds_not_reported_names) %>%
      dplyr::mutate(nav = -1*cash_flow) %>%
      dplyr::select(-cash_flow) %>%
      dplyr::mutate(effective_date = lubridate::as_date(end_date)) %>%
      dplyr::bind_rows(funds_not_reported_data %>% dplyr::mutate(effective_date = lubridate::as_date(end_date), nav = -1*nav)) %>%
      dplyr::group_by(pm_fund_id, effective_date) %>%
      dplyr::summarize(nav = sum(nav)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cash_flow = 0) %>%
      dplyr::left_join(pm_fund_info, by = 'pm_fund_id')


    # append cash adjusted nav to not reported nav
    .nav_data = .nav_data %>% bind_rows(cf_as_nav)

    print(.nav_data %>% filter(pm_fund_id == 'AveAZ') %>% select(pm_fund_id, effective_date, cash_flow, nav))
    print(.cf_data %>% filter(pm_fund_id == 'AveAZ') %>% select(pm_fund_id, effective_date, cash_flow, nav))
    print(dplyr::union_all(.nav_data, .cf_data) %>% filter(pm_fund_id == 'AveAZ') %>% select(pm_fund_id, effective_date, cash_flow, nav))

    return(dplyr::union_all(.nav_data, .cf_data))
    }

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

my_calcs = function(start_date, end_date, itd, cash_adjusted, pm_fund_info, ...){

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

  clean_data = merge_nav_and_cf(nav1, cf1, end_date = end_date, cash_adjusted = cash_adjusted, pm_fund_info = pm_fund_info) %>%
    filter_dates(start_date = start_date, end_date = end_date, itd = itd, ...) %>%
    clean_nav_cf(pm_fund_info = pm_fund_info)

  clean_data %>%
    calculate_grouped_irr(...)
  }


nav = get_pm_nav_daily() %>% dplyr::filter(nav != 0)
cf = get_pm_cash_flow_daily() %>% dplyr::filter(cash_flow != 0)
pm_fund_info = get_pm_fund_info()

start_date = '2019-09-30'
end_date = '2019-12-31'
itd = FALSE
cash_adjusted = TRUE

d = my_calcs(start_date = start_date, end_date = end_date, itd = itd,
             cash_adjusted = cash_adjusted, pm_fund_info = pm_fund_info,
         pm_fund_portfolio, pm_fund_category_description)

d
