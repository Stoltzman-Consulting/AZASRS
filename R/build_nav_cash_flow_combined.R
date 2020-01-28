#' Get and combine cash flows and nav according to irr.z calc
#'
#' @export
build_nav_cash_flow_combined = function(...,
                                        con = AZASRS_DATABASE_CONNECTION(),
                                        start_date = '2019-06-30',
                                        end_date = get_value_date(con = con),
                                        nav_daily = get_pm_nav_daily(con = con, return_tibble = FALSE),
                                        cash_flow_daily = get_pm_cash_flow_daily(con = con, return_tibble = FALSE),
                                        itd = FALSE,
                                        return_tibble = FALSE){

  min_max_nav_dates = nav_daily %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(min_nav_date = min(effective_date, na.rm = TRUE),
                     max_nav_date = max(effective_date, na.rm = TRUE)) %>%
    dplyr::ungroup()

  max_non_zero_nav_dates = nav_daily %>%
    dplyr::filter(nav != 0) %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(max_non_zero_nav_date = max(effective_date, na.rm = TRUE)) %>%
    dplyr::ungroup()

  if(itd == TRUE){
    nav = nav_daily %>%
      dplyr::left_join(min_max_nav_dates) %>%
      dplyr::left_join(max_non_zero_nav_dates) %>%
      dplyr::filter(effective_date == min_nav_date | effective_date == max_non_zero_nav_date) %>%
      dplyr::group_by(..., effective_date) %>%
      dplyr::summarize(nav = sum(nav, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(...) %>%
      dplyr::mutate(min_date = min(effective_date, na.rm = TRUE),
                    max_date = end_date) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(nav = dplyr::if_else(effective_date == min_date, -1*nav, nav))

  } else{
    nav = nav_daily %>%
      dplyr::left_join(min_max_nav_dates) %>%
      # dplyr::mutate(nav = dplyr::if_else(is.na(min_nav_date), 0, nav)) %>%
      dplyr::filter(start_date >= min_nav_date, effective_date <= max_nav_date) %>%
      dplyr::filter(effective_date == start_date | effective_date == end_date) %>%
      dplyr::group_by(...) %>%
      dplyr::mutate(n = dplyr::count()) %>%
      dplyr::filter(n == 2) %>%
      dplyr::select(-n) %>%
      dplyr::group_by(..., effective_date) %>%
      dplyr::summarize(nav = sum(nav, na.rm = TRUE)) %>%
      dplyr::mutate(nav = dplyr::if_else(effective_date == start_date, -1*nav, nav))
  }

  cf_date_filter = min_max_nav_dates %>%
    dplyr::rename(min_date = min_nav_date,
                  max_date = max_nav_date)

  cf =  cf_date_filter %>%
    dplyr::left_join(cash_flow_daily) %>%
    dplyr::filter(effective_date >= start_date, effective_date < end_date) %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(cash_flow = sum(cash_flow, na.rm = TRUE))

  # Prep for union
  nav = nav %>%
    dplyr::mutate(cash_flow = 0)
  cf = cf %>%
    dplyr::mutate(nav = 0)

  dat = dplyr::union_all(nav, cf) %>%
    dplyr::mutate(nav = dplyr::if_else(is.na(nav), 0, nav),
                  cash_flow = dplyr::if_else(is.na(cash_flow), 0, cash_flow)) %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(nav_cf = sum(nav, na.rm = TRUE) + sum(cash_flow, na.rm = TRUE)) %>%
    dplyr::arrange(..., effective_date) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(distributions = dplyr::if_else(nav_cf > 0, nav_cf, 0),
           contributions = dplyr::if_else(nav_cf < 0, nav_cf, 0)) %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(nav = if_else(effective_date == min(effective_date, na.rm = TRUE) | effective_date == max(effective_date, na.rm = TRUE), nav_cf, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(nav_cf = sum(nav_cf, na.rm = TRUE),
                     distributions = sum(distributions, na.rm = TRUE),
                     contributions = sum(contributions, na.rm = TRUE),
                     nav = sum(nav, na.rm = TRUE)) %>%
    dplyr::ungroup()


  if(return_tibble){
    return(dat %>% tibble::as_tibble())
  } else{
    return(dat)
  }
}



