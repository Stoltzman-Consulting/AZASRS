#' Get and combine cash flows and nav according to irr.z calc
#'
#' @export
build_nav_cash_flow_combined = function(...,
                                        con = AZASRS_DATABASE_CONNECTION(),
                                        start_date = '2004-12-31',
                                        end_date = get_value_date(con = con),
                                        nav_daily = get_pm_nav_daily(con = con, return_tibble = FALSE),
                                        cash_flow_daily = get_pm_cash_flow_daily(con = con, return_tibble = FALSE),
                                        return_tibble = FALSE){

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
    dplyr::arrange(..., effective_date)

  if(return_tibble){
    return(dat %>% tibble::as_tibble())
  } else{
    return(dat)
  }
}



