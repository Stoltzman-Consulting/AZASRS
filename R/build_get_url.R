#' Build url to grab data from server- metrics or info
#'
#' @description Builds url to grab data
#' @param data_request is the dataframe desired (MUST BE IN QUOTES).. pm_fund_info, benchmark_index etc.
#' @examples
#' build_get_url("benchmark_info")
#' @export

build_get_url = function(data_request){
  exists(BASE_URL)
  exists(DATA_RETRIEVAL_TOKEN)

  return(paste0(BASE_URL, data_request, "/", DATA_RETRIEVAL_TOKEN))
}
