#' Get all benchmark_info
#'
#' @description A view to get all private market benchmark info
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or data.frame result with all benchmark_info metadata.
#' @examples
#' get_benchmark_info()
#' @export
get_benchmark_info <- function(return_tibble = TRUE) {

  dat <- get_url_data("benchmark_info")

  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
