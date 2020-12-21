#' Get all pm_fund_nav_quarterly
#'
#' @description A view to get all private market fund nav quarterly, can be filtered. By default, SQL --> SELECT * FROM all_pm_fund_info;
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or data.frame result with all pm_fund_nav_quarterly metadata.
#' @examples
#' get_pm_fund_nav_quarterly()
#' @export
get_pm_fund_nav_quarterly <- function(return_tibble = TRUE) {

  dat <- get_url_data("pm_fund_nav_quarterly")

  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
