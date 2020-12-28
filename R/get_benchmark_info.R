#' Get all benchmark_info
#'
#' @description A view to get all private market benchmark info
#' @return Returns a tibble or data.frame result with all benchmark_info metadata.
#' @examples
#' get_benchmark_info()
#' @export
get_benchmark_info <- function() {

  dat <- get_url_data("benchmark_info")

  return(dat)
}
