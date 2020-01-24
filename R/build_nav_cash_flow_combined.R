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
    nav = nav_daily %>%
      dplyr::group_by(...) %>%
      dplyr::filter(nav != 0) %>%
      dplyr::mutate(min_date = min(effective_date, na.rm = TRUE),
                    max_date = max(effective_date, na.rm = TRUE)) %>%
      dplyr::filter(effective_date == min_date | effective_date == max_date) %>%
      dplyr::group_by(..., effective_date) %>%
      dplyr::summarize(nav = sum(nav, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(...) %>%
      dplyr::mutate(min_date = min(effective_date, na.rm = TRUE),
                    max_date = max(effective_date, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(nav = dplyr::if_else(effective_date == min_date, -1*nav, nav))

  } else{
    nav = nav_daily %>%
      dplyr::filter(effective_date == start_date | effective_date == end_date) %>%
      dplyr::group_by(..., effective_date) %>%
      dplyr::summarize(nav = sum(nav, na.rm = TRUE)) %>%
      dplyr::mutate(nav = dplyr::if_else(effective_date == start_date, -1*nav, nav))
  }

  cf_date_filter = nav %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(min_date = min(effective_date, na.rm = TRUE),
                     max_date = end_date) %>%
    dplyr::ungroup()

  cf = cash_flow_daily %>%
    dplyr::left_join(cf_date_filter) %>%
    dplyr::filter(effective_date >= min_date, effective_date < max_date) %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(cash_flow = sum(cash_flow, na.rm = TRUE))

  # Prep for union
  nav = nav %>%
    dplyr::mutate(cash_flow = 0) %>%
    dplyr::select(-min_date, -max_date)
  cf = cf %>%
    dplyr::mutate(nav = 0)

  dat = dplyr::union_all(nav, cf) %>%
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



