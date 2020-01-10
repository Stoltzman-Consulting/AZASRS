#' Get and combine cash flows and nav according to irr.z calc
#'
#' @export
build_nav_cash_flow_combined = function(...,
                                        con = AZASRS_DATABASE_CONNECTION(),
                                        start_date = '2017-12-31',
                                        end_date = get_value_date(con = con),
                                        nav_daily = get_pm_nav_daily(con = con, return_tibble = FALSE),
                                        cash_flow_daily = get_pm_cash_flow_daily(con = con, return_tibble = FALSE),
                                        itd = FALSE,
                                        return_tibble = FALSE){

  if(itd){
    start_date = '1900-12-31'

    qtr_dates = filled_list_of_dates(start_date = start_date, end_date = end_date)$date

    nav = nav_daily %>%
      dplyr::filter(effective_date %in% qtr_dates) %>%
      dplyr::filter(effective_date >= start_date, effective_date <= end_date)

    cf = cash_flow_daily %>% dplyr::filter(effective_date >= start_date, effective_date < end_date)

    min_max_nav_dates = nav %>%
      dplyr::group_by(pm_fund_portfolio) %>%
      dplyr::summarize(min_date = min(effective_date, na.rm = TRUE),
                max_date = max(effective_date, na.rm = TRUE)) %>%
      dplyr::ungroup()

    min_max_nav = nav %>%
      dplyr::left_join(min_max_nav_dates) %>%
      dplyr::filter(effective_date == min_date | effective_date == max_date) %>%
      dplyr::group_by(pm_fund_portfolio, effective_date) %>%
      dplyr::summarize(nav_cf = sum(nav, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(pm_fund_portfolio) %>%
      dplyr::mutate(min_date = min(effective_date, na.rm = TRUE),
                    max_date = max(effective_date, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(nav_cf = dplyr::if_else(effective_date == min_date, -1*nav_cf, nav_cf))

    min_max_cf = cf %>%
      dplyr::group_by(pm_fund_portfolio, effective_date) %>%
      dplyr::summarize(nav_cf = sum(cash_flow, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(min_max_nav %>% dplyr::select(-nav_cf, -effective_date)) %>%
      dplyr::filter(effective_date >= min_date, effective_date < max_date)


    dat = dplyr::union_all(min_max_nav, min_max_cf) %>%
      dplyr::group_by(pm_fund_portfolio, effective_date) %>%
      dplyr::summarize(nav_cf = sum(nav_cf, na.rm = TRUE),
                       min_date = min(min_date, na.rm = TRUE),
                       max_date = max(max_date, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(distributions = dplyr::if_else(nav_cf > 0, nav_cf, 0),
                    contributions = dplyr::if_else(nav_cf < 0, nav_cf, 0),
                    nav = if_else(effective_date == min_date | effective_date == max_date, nav_cf, 0)) %>%
      dplyr::arrange(pm_fund_portfolio, effective_date) %>%
      dplyr::ungroup() %>%
      dplyr::select(-min_date, -max_date)

  } else{

  nav = nav_daily %>%
    dplyr::filter(effective_date == start_date | effective_date == end_date) %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(nav_cf = sum(nav, na.rm = TRUE)) %>%
    dplyr::mutate(nav_cf = dplyr::if_else(effective_date == start_date, -1*nav_cf, nav_cf))

  cf = cash_flow_daily %>%
    dplyr::filter(effective_date >= start_date, effective_date < end_date) %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(nav_cf = sum(cash_flow, na.rm = TRUE))

  dat = dplyr::union_all(nav, cf) %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(nav_cf = sum(nav_cf, na.rm = TRUE)) %>%
    dplyr::arrange(..., effective_date) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(distributions = dplyr::if_else(nav_cf > 0, nav_cf, 0),
           contributions = dplyr::if_else(nav_cf < 0, nav_cf, 0),
           nav = if_else(effective_date == start_date | effective_date == end_date, nav_cf, 0))
  }

  if(return_tibble){
    return(dat %>% tibble::as_tibble())
  } else{
    return(dat)
  }
}



