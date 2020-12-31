#' Get all pm_fund_info
#'
#' @description A view to get all private market fund info, can be filtered. By default, SQL --> SELECT * FROM all_pm_fund_info;
#' @return Returns a tibble or data.frame result with all pm_fund_info metadata.
#' @examples
#' get_pm_fund_info_raw()
#' @export
get_pm_fund_info_raw <- function() {

  dat <- get_url_data("pm_fund_info")

  return(dat)
}


#add benchmark code here
get_pm_fund_info <- function(add_benchmark = FALSE) {

  dat <- get_pm_fund_info_raw()

  return(dat)
}
