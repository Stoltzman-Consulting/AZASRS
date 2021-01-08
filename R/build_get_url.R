#' Build url to grab data from server- metrics or info
#'
#' @description Builds url to grab data
#' @param data_request is the dataframe desired (MUST BE IN QUOTES).. pm_fund_info, benchmark_index etc.
#' @examples
#' build_get_url("benchmark_info")
#' @export

build_get_url = function(data_request){

  stopifnot("AZASRS_BASE_URL does not exist, check .Renviron file" = !identical(Sys.getenv("AZASRS_BASE_URL"), ''))
  stopifnot("AZASRS_DATA_RETRIEVAL_TOKEN does not exist, check .Renviron file" = !identical(Sys.getenv("AZASRS_DATA_RETRIEVAL_TOKEN"), ''))

  return(paste0(Sys.getenv("AZASRS_BASE_URL"), "?token=", Sys.getenv("AZASRS_DATA_RETRIEVAL_TOKEN"), "&data=", data_request, ".csv"))

}
