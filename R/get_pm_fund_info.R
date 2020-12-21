#' Get all pm_fund_info
#'
#' @description A view to get all private market fund info, can be filtered. By default, SQL --> SELECT * FROM all_pm_fund_info;
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or data.frame result with all pm_fund_info metadata.
#' @examples
#' get_pm_fund_info()
#' @export
get_pm_fund_info <- function(return_tibble = TRUE) {

  dat <- get_url_data("pm_fund_info")

  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
