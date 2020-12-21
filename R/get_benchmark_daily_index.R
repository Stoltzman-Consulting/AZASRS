#' Get all benchmark_daily_index
#'
#' @description Gets benchmark index data by day
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or data.frame object.
#' @examples
#' get_benchmark_daily_index(return_tibble = TRUE)
#' @export
get_benchmark_daily_index <- function(return_tibble = TRUE) {

  dat <- get_url_data("benchmark_daily_index")

  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
