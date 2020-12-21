#' Get constants
#'
#' @description A view to get constants
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or data.frame result with all constants
#' @examples
#' get_constants()
#' @export
get_constants <- function(return_tibble = TRUE) {

  dat <- get_url_data("constants")

  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
