#' Get relationships between funds and benchmarks get_benchmark_fund_relationship
#'
#' @description Finds all data from benchmark_info_id matched with pm_fund_info_id
#' @param con is the db connection, default of AZASRS_DATABASE_CONNECTION()
#' @return Returns a table of relationships of ALL types, not filtered
#' @export
get_benchmark_fund_relationship = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble = FALSE){

  dat = tbl_pm_fund_info_benchmark_info(con) %>%
    dplyr::left_join(tbl_benchmark_type(con), by = 'benchmark_type_id') %>%
    dplyr::select(pm_fund_info_id, benchmark_info_id, benchmark_type)

  if(return_tibble){
    return(dat %>% tibble::as_tibble())
  } else{
    return(dat)
  }

}
