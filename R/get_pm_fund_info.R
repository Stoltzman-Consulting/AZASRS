#' Get all pm_fund_info RAW
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

#' Get all pm_fund_info
#'
#' @description A view to get all private market fund info, can be filtered. By default, SQL --> SELECT * FROM all_pm_fund_info;
#' @param con  default TRUE, needed to keep consistent with older version
#' @param add_benchmark default FALSE, needed to keep consistent with older version
#' @param return_tibble default TRUE, needed to keep consistent with older version
#' @return Returns a tibble or data.frame result with all pm_fund_info metadata.
#' @examples
#' get_pm_fund_info()
#' @export
get_pm_fund_info <- function(con = TRUE, add_benchmark = FALSE, return_tibble = TRUE) {

  dat <- get_pm_fund_info_raw()
  return(dat)

  }


