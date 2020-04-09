#' Create rollup of IRR based off of dates
#'
#' @description Build IRR for any rollup and date range, including ITD and cash adjusted
#' @param .data is from clean_nav_cf()
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
#' @examples # Example use case
#' nav = get_pm_nav_daily() %>% dplyr::filter(nav != 0)
#' cf = get_pm_cash_flow_daily() %>% dplyr::filter(cash_flow != 0)
#' pm_fund_info = get_pm_fund_info()
#' start_date = '2019-09-30'
#' end_date = '2019-12-31'
#' itd = FALSE
#' cash_adjusted = FALSE
#' final_data = my_calcs(start_date = start_date, end_date = end_date, itd = itd,
#'                       cash_adjusted = cash_adjusted, pm_fund_info = pm_fund_info,
#'                       pm_fund_portfolio, pm_fund_category_description)
#' @export
build_grouped_irrs = function(start_date, end_date, itd, cash_adjusted, pm_fund_info, ...){

  # ITD failsafe - ensure start_date is before earliest possible pm_fund_cash_flow
  if(itd){
    start_date = '2004-06-30'
  }

  nav_prep = nav %>%
    filter_nav_on_dates(start_date = start_date, end_date = end_date, itd = itd) %>%
    append_nav_has_reported(end_date = end_date) %>%
    convert_start_date_nav_to_negative(start_date = start_date, itd = itd)

  cf_prep = cf %>%
    get_cf_between_dates(start_date = start_date, end_date = end_date, itd = itd)

  clean_data = merge_nav_and_cf(nav_prep, cf_prep, end_date = end_date, cash_adjusted = cash_adjusted, pm_fund_info = pm_fund_info) %>%
    filter_dates(start_date = start_date, end_date = end_date, itd = itd, ...) %>%
    clean_nav_cf(pm_fund_info = pm_fund_info)

  clean_data %>%
    calculate_grouped_irr(...)
}

# All of the following functions simply only serve the use case to calculate the grouped IRR above.

#' Filter Private market NAV on dates
#'
#' @description Finds start / end date NAV and can utilize ITD
#' @param .data is from get_pm_nav_daily()
#' @param start_date is a string (format yyyy-dd-mm)
#' @param end_date is a string (format yyyy-dd-mm)
#' @param itd TRUE / FALSE for itd (overwrites start_date if TRUE)
filter_nav_on_dates = function(.data, start_date, end_date, itd){
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


#' Append column if NAV has reported at end date
#'
#' @description Looks for NAV at end_date and appends TRUE / FALSE column
#' @param .data is from get_pm_nav_daily() after filter_nav_on_dates()
#' @param end_date is a string (format yyyy-dd-mm)
append_nav_has_reported <- function(.data, end_date){
  .data %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(has_reported = max(effective_date) == end_date) %>%
    dplyr::ungroup() %>%
    dplyr::select(has_reported, dplyr::everything())
}


#' Convert first NAV to negative
#'
#' @description Simulates cash flow at start_date from NAV
#' @param .data is from get_pm_nav_daily() after append_nav_has_reported()
#' @param start_date is a string (format yyyy-dd-mm)
#' @param itd TRUE / FALSE for itd (overwrites start_date if TRUE)
convert_start_date_nav_to_negative = function(.data, start_date){
  # The -1*nav at the start date stands in as a cash flow
  .data %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(nav = dplyr::if_else(effective_date == start_date,
                                       -1*nav,
                                       nav)) %>%
    dplyr::ungroup()
}


#' Collect cash flow between dates
#'
#' @description Includes start and end dates, allows for ITD
#' @param .data is from get_pm_cash_flow_daily()
#' @param start_date is a string (format yyyy-dd-mm)
#' @param end_date is a string (format yyyy-dd-mm)
#' @param itd TRUE / FALSE for itd (overwrites start_date if TRUE)
filter_cf_between_dates = function(.data, start_date, end_date, itd){
  if(itd){
    .data %>%
      dplyr::filter(effective_date <= end_date)
  } else{
  .data %>%
    dplyr::filter(effective_date >= start_date,
                  effective_date <= end_date)
  }
}


#' Merge NAV and CF
#'
#' @description Merging both NAV and CF and handle complexity of rollup aggregation
#' @param .nav_data is from get_pm_nav_daily() through convert_start_date to negative()
#' @param .cf_data is from get_pm_cash_flow_daily() through filter_cf_between_dates
#' @param end_date is a string (format yyyy-dd-mm)
#' @param cash_adjusted is TRUE / FALSE to allow for cash adjusted NAV at next quarter
#' @param pm_fund_info from get_pm_fund_info()
merge_nav_and_cf = function(.nav_data, .cf_data, end_date, cash_adjusted, pm_fund_info){

  # Add a has_reported field to use as a filter later on
  .nav_data = .nav_data %>% dplyr::mutate(cash_flow = 0)
  .cf_data = .cf_data %>% dplyr::mutate(nav = 0)

  # Not cash adjusted means to ONLY use funds that have NAV at end_date
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
    # Replace all dates with end date to simulate end_date NAV
    # Reverse signs to signal increase in NAV "contributions become distributions in a sense"
    # Bind rows to previous NAV to sum cash flow on top of start_date NAV (reversed sign because it was flipped earlier in pipeline)
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
    return(dplyr::union_all(.nav_data, .cf_data))
    }

  }


#' Filter dates from NAV / CF joined
#'
#' @description NAV / CF joined from merge_nav_and_cf() needs to filter out dates IF not aggregated and not ITD. This avoids filtering out important data from roll up calculations
#' @param .data is from merge_nav_and_cf()
#' @param start_date is a string (format yyyy-dd-mm)
#' @param end_date is a string (format yyyy-dd-mm)
#' @param itd is TRUE / FALSE for inception to date
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
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


#' Cleans NAV / CF filtered by date data to sum up by pm_fund_id
#'
#' @description NAV / CF joined from filter_dates()
#' @param .data is from filter_dates()
#' @param pm_fund_info from get_pm_fund_info()
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


#' Calculate the rollup IRR
#'
#' @description Calculates the IRR from a list of "cash flow" from clean_nav_cf()
#' @param .data is from clean_nav_cf()
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
calculate_grouped_irr = function(.data, ...){
  .data %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(adjusted_cash_flow = sum(adjusted_cash_flow, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(irr = calc_irr(adjusted_cash_flow, effective_date))
}
