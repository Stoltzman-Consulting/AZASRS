#' Get all benchmark_daily_index
#'
#' @description Gets benchmark index data by day
#' @return Returns a tibble or data.frame object.
#' @examples
#' get_benchmark_daily_index(return_tibble = TRUE)
#' @export
get_benchmark_daily_index <- function() {

  dat <- get_url_data("benchmark_daily_index")

  return(dat)
}
