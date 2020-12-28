#' Get value date
#'
#' @description Finds the value date based off of the constants table from most recent database population
#' @return string of YYYY-mm-dd of value date as set by ASRS
#' @examples
#' get_value_date()
#' @export
get_value_date <- function() {

  dat <- get_url_data("constants")
  final_dat <- dat$value_date

  return(final_dat)

}
