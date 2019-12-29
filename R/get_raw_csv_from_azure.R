#' Get CSV Data from Azure
#'
#' @description Gets RAW csv for IRR comparison from Azure
#' @param data_directory is a relative path to specific folder where all CSV files must be loaded
#' @export
get_raw_csv_from_azure = function(data_directory = './reconciliation_data/'){

  cont = AzureStor::blob_container(Sys.getenv('ASRS_BLOB_PM_RECONCILIATION'), key = Sys.getenv('ASRS_BLOB_KEY'))
  all_files = AzureStor::list_blobs(cont)
  AzureStor::multidownload_blob(cont, '*.csv', data_directory, overwrite = TRUE)

  csv_irrs = tibble::tibble()
  for(file in all_files$name){
    tmp = readr::read_csv(paste0(data_directory, file)) %>%
      dplyr::select(`Client Grouping 1`, `3 Month IRR`, `6 Month IRR`,
             `1 Year IRR`, `3 Year IRR`, `5 Year IRR`, `7 Year IRR`, `10 Year IRR`, `ITD IRR`) %>%
      tidyr::drop_na(`Client Grouping 1`)
    csv_irrs = dplyr::bind_rows(csv_irrs, tmp)
  return(csv_irrs)
  }
}
