#' Gets data from server
#'
#' @description Gets data from server - based on url from build_get_url()
#' @param data_request character dataframe desired (MUST BE IN QUOTES).. "pm_fund_info", "benchmark_index" etc.
#' @examples
#' get_url_data("benchmark_info")
#' @return data.frame
#' @export


get_url_data = function(data_request){

  DATA_URL <- build_get_url(data_request)

  data = readr::read_csv(DATA_URL)
}

