#' Daily private market cash flows
#'
#' @description A view to get all private market cash flow daily, can be filtered. By default, SQL --> SELECT * FROM all_pm_fund_info;
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or data.frame result with all pm_fund_cash_flow_daily metadata.
#' @examples
#' get_pm_fund_cash_flow_daily()
#' @export
get_pm_fund_cash_flow_daily <- function(return_tibble = TRUE) {

  dat <- get_url_data("pm_fund_cash_flow_daily")

  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
