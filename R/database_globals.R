
# Global variables
#' @export
#AZASRS_DATABASE_LOCATION = "P:\\IMD\\2018 Database Project\\Database\\asrs_database.db"
AZASRS_DATABASE_LOCATION = "C:\\Users\\scotts\\Desktop\\2018 Database Project\\Database\\asrs_database.db" ##For local development only

#' @export
AZASRS_DATABASE_CONNECTION = function(){ return(dplyr::src_sqlite(AZASRS_DATABASE_LOCATION)) }

#' @export
AZASRS_TEST_DATA_DIRECTORY = "P:/IMD/2018 Database Project/Application Data/etl_check_data/"

#' @export
SHOW_ALL_TABLES = print(dplyr::src_tbls(AZASRS_DATABASE_CONNECTION()))

# All existing views
#' @export
account_info = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat = dplyr::tbl(con, "all_account_info")
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

#' @export
ssbt_composite_info = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat = dplyr::tbl(con, "all_ssbt_composite_info")
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

#' @export
pm_fund_info = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat = dplyr::tbl(con, "all_pm_fund_info")
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

#' @export
benchmark_daily_return = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat = tbl_benchmark_daily_return(con) %>%
    left_join(tbl_benchmark_info(con), by = 'benchmark_info_id')
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

#' @export
benchmark_monthly_return = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat = tbl_benchmark_monthly_return(con) %>%
    left_join(tbl_benchmark_info(con), by = 'benchmark_info_id')
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

#' @export
daily_data_account = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat = dplyr::tbl(con, "daily_data_account")
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

#' @export
daily_data_composite = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat = dplyr::tbl(con, "daily_data_composite")
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

#' @export
monthly_data_account = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat = dplyr::tbl(con, "monthly_data_account")
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

#' @export
monthly_data_composite = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat = dplyr::tbl(con, "monthly_data_composite")
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

# All existing tables
#' @export
tbl_account_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "account_info")}

#' @export
tbl_asset_class = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "asset_class")}

#' @export
tbl_benchmark_daily_return = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "benchmark_daily_return")}

#' @export
tbl_benchmark_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "benchmark_info")}

#' @export
tbl_benchmark_monthly_return = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "benchmark_monthly_return")}

#' @export
tbl_book_of_record_account_daily = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "book_of_record_account_daily")}

#' @export
tbl_book_of_record_account_monthly = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "book_of_record_account_monthly")}

#' @export
tbl_book_of_record_composite_daily = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "book_of_record_composite_daily")}

#' @export
tbl_book_of_record_composite_monthly = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "book_of_record_composite_monthly")}

#' @export
tbl_category = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "category")}

#' @export
tbl_city = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "city")}

#' @export
tbl_pm_cash_flow_daily = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_cash_flow_daily")}

#' @export
tbl_pm_fund_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_fund_info")}

#' @export
tbl_pm_nav_daily = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_nav_daily")}

#' @export
tbl_portfolio = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "portfolio")}

#' @export
tbl_previous_saa = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "previous_saa")}

#' @export
tbl_sponsor = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "sponsor")}

#' @export
tbl_ssbt_composite_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "ssbt_composite_info")}

#' @export
tbl_ssbt_composite_info_account_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "ssbt_composite_info_account_info")}

#' @export
tbl_sub_portfolio = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "sub_portfolio")}
