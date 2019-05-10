
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

#' @export
AZASRS_DATABASE_DISCONNECT = function(con){ DBI::dbDisconnect(con$con) }

# All existing views
#' @export
account_info = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat = dplyr::tbl(con, "all_account_info")
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
    dplyr::left_join(tbl_benchmark_info(con), by = 'benchmark_info_id')
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

#' @export
benchmark_monthly_return = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat = tbl_benchmark_monthly_return(con) %>%
    dplyr::left_join(tbl_benchmark_info(con), by = 'benchmark_info_id')
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

#' @export
account_bor_daily = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat =  tbl_account_book_of_record_daily(con) %>%
    dplyr::left_join(account_info(con, return_tibble = FALSE), by = 'account_info_id')
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

#' @export
account_bor_monthly = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat =  tbl_account_book_of_record_monthly(con) %>%
    dplyr::left_join(account_info(con, return_tibble = FALSE), by = 'account_info_id')
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

#' @export
ssbt_composite_bor_daily = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat = tbl_ssbt_composite_book_of_record_daily(con) %>%
    dplyr::left_join(tbl_ssbt_composite_info(con), by = 'ssbt_composite_info_id')
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

#' @export
ssbt_composite_bor_monthly = function(con = AZASRS_DATABASE_CONNECTION(), return_tibble=TRUE){
  dat = tbl_ssbt_composite_book_of_record_monthly(con) %>%
    dplyr::left_join(tbl_ssbt_composite_info(con), by = 'ssbt_composite_info_id')
  if(return_tibble == TRUE){ dat = dat %>% tibble::as_tibble() }
  return(dat)}

#' @export
value_date = function(con = AZASRS_DATABASE_CONNECTION()){
  dat = tbl_constants(con) %>% tibble::as_tibble()
  dat = dat$value_date
  return(dat)}

#' @export
next_quarter = function(con = AZASRS_DATABASE_CONNECTION()){
  dat = tbl_constants(con) %>% tibble::as_tibble()
  dat = dat$next_quarter
  return(dat)}

# All existing tables
#' @export
tbl_account_asset_class = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "account_asset_class")}

#' @export
tbl_account_book_of_record_daily = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "account_book_of_record_daily")}

#' @export
tbl_account_book_of_record_monthly = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "account_book_of_record_monthly")}

#' @export
tbl_account_category = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "account_category")}

#' @export
tbl_account_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "account_info")}

#' @export
tbl_account_info_benchmark_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "account_info_benchmark_info")}

#' @export
tbl_account_info_pm_fund_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "account_info_pm_fund_info")}

#' @export
tbl_account_portfolio = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "account_portfolio")}

#' @export
tbl_account_sponsor = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "account_sponsor")}

#' @export
tbl_account_sub_portfolio = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "account_sub_portfolio")}

#' @export
tbl_benchmark_daily_return = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "benchmark_daily_return")}

#' @export
tbl_benchmark_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "benchmark_info")}

#' @export
tbl_benchmark_monthly_return = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "benchmark_monthly_return")}

#' @export
tbl_benchmark_type_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "benchmark)_type_info")}

#' @export
tbl_constants = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "constants")}

#' @export
tbl_pm_fund_cash_flow_daily = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_fund_cash_flow_daily")}

#' @export
tbl_pm_fund_category = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_fund_category")}

#' @export
tbl_pm_fund_city = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_fund_city")}

#' @export
tbl_pm_fund_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_fund_info")}

#' @export
tbl_pm_fund_info_benchmark_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_fund_info_benchmark_info")}

#' @export
tbl_pm_fund_nav_daily = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_fund_nav_daily")}

#' @export
tbl_pm_fund_portfolio = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_fund_portfolio")}

#' @export
tbl_pm_fund_sponsor = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_fund_sponsor")}

#' @export
tbl_ssbt_composite_book_of_record_daily = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "ssbt_composite_book_of_record_daily")}

#' @export
tbl_ssbt_composite_book_of_record_monthly = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "ssbt_composite_book_of_record_monthly")}

#' @export
tbl_ssbt_composite_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "ssbt_composite_info")}

#' @export
tbl_ssbt_composite_info_account_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "ssbt_composite_info_account_info")}

#' @export
tbl_ssbt_composite_info_benchmark_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "ssbt_composite_info_benchmark_info")}

#' @export
tbl_sub_portfolio = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "sub_portfolio")}
