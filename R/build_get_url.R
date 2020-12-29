#' Build url to grab data from server- metrics or info
#'
#' @description Builds url to grab data
#' @param data_request is the dataframe desired (MUST BE IN QUOTES).. pm_fund_info, benchmark_index etc.
#' @examples
#' build_get_url("benchmark_info")
#' @export

build_get_url = function(data_request){

  AZASRS_BASE_URL <- Sys.getenv("AZASRS_BASE_URL")
  AZASRS_DATA_RETRIEVAL_TOKEN <- Sys.getenv("AZASRS_DATA_RETRIEVAL_TOKEN")

  if(exists("AZASRS_BASE_URL") && exists("AZASRS_DATA_RETRIEVAL_TOKEN")) {
    return(paste0(Sys.getenv("AZASRS_BASE_URL"), data_request, "/", Sys.getenv("AZASRS_DATA_RETRIEVAL_TOKEN")))
  } else {
    print("Make sure token and URL are in .Renviron file")
  }


}
