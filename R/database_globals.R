# Global variables - to be used for extremely basic functions interacting with low level connections to the database

#' Database location - should reflect production database "shared" location
#'
#' @examples
#' "P:/IMD/2018 Database Project/Database/asrs_database.db"
#' @export
AZASRS_DATABASE_LOCATION = "P:\\IMD\\2018 Database Project\\Database\\asrs_database.db"
#AZASRS_DATABASE_LOCATION = "C:\\Users\\scotts\\Desktop\\2018 Database Project\\Database\\asrs_database.db" ##For local development only


#' Opens database connection
#' @description Uses AZASRS_DATABASE_LOCATION and should only be used with tbl_ functions. Allows for chaining of tbl_ functions to optimize SQL queries. Must close connection after usage.
#' @examples
#' con = AZASRS_DATABASE_CONNECTION()
#' data = tbl_pm_fund_nav_daily(con) %>%
#'        left_join(tbl_pm_fund_info(con), by = 'pm_fund_id') %>%
#'        as_tibble()
#' AZASRS_DATABASE_DISCONNECT(con)
#' @export
AZASRS_DATABASE_CONNECTION = function(){ return(dplyr::src_postgres(dbname = Sys.getenv('ASRS_DATABASE'), host = Sys.getenv('ASRS_HOST'),
                                                                    port = Sys.getenv('ASRS_PORT'), user = Sys.getenv('ASRS_USER'),
                                                                    password = Sys.getenv('ASRS_PASSWORD'))) }


#' @export
UPDATE_DATABASE = function(filename){
  r = httr::GET(paste0('https://populate-database.azurewebsites.net/api/HttpTrigger?code=', Sys.getenv('ASRS_FUNCTIONS_CODE'),
                       '&username=', Sys.getenv('ASRS_USER'),
                       '&password=', Sys.getenv('ASRS_PASSWORD'),
                       '&account_name=asrs',
                       '&account_key=', Sys.getenv('ASRS_FUNCTIONS_KEY'),
                       '&filename=', filename))
  return(r)
}


#' Location of test data
#' @description .rds files saved and read from this location for tests
#' @export
AZASRS_TEST_DATA_DIRECTORY = "P:/IMD/2018 Database Project/Application Data/etl_check_data/"


#' List all tables and views in database
#' @description Aids in displaying table names, simply add tbl_ in front of the name to access the function that accesses the table
#' @export
SHOW_ALL_TABLES = print(dplyr::src_tbls(AZASRS_DATABASE_CONNECTION()))


#' Disconnect from database
#' @description Disconnect from database if using AZASRS_DATABASE_CONNECTION
#' @examples
#' con = AZASRS_DATABASE_CONNECTION()
#' data = tbl_pm_fund_nav_daily(con) %>%
#'        left_join(tbl_pm_fund_info(con), by = 'pm_fund_id') %>%
#'        as_tibble()
#' AZASRS_DATABASE_DISCONNECT(con)
#' @export
AZASRS_DATABASE_DISCONNECT = function(con){ DBI::dbDisconnect(con$con) }


#' Get value date
#'
#' @description Finds the value date based off of the constants table from most recent database population
#' @export
get_value_date = function(con = AZASRS_DATABASE_CONNECTION()){
  dat = tbl_constants(con) %>% tibble::as_tibble()
  dat = dat$value_date
  return(dat)}


#' Get next quarter date
#'
#' @description Finds the next quarter date based off of constants table from most recent database population
#' @export
get_next_quarter = function(con = AZASRS_DATABASE_CONNECTION()){
  dat = tbl_constants(con) %>% tibble::as_tibble()
  dat = dat$next_quarter
  return(dat)}


# tbl_ allows access to optimize queries from database while using tidyverse style. These ONLY refer to raw tables and not views.

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
tbl_benchmark_daily_index = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "benchmark_daily_index")}

#' @export
tbl_benchmark_daily_return = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "benchmark_daily_return")}

#' @export
tbl_benchmark_info = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "benchmark_info")}

#' @export
tbl_benchmark_monthly_return = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "benchmark_monthly_return")}

#' @export
tbl_benchmark_type = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "benchmark_type")}

#' @export
tbl_constants = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "constants")}

#' @export
tbl_pm_fund_cash_flow_daily = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_fund_cash_flow_daily")}

#' @export
tbl_pm_fund_category = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_fund_category")}

#' @export
tbl_pm_fund_category_description = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_fund_category_description")}

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
tbl_pm_fund_sector = function(con = AZASRS_DATABASE_CONNECTION()){dplyr::tbl(con, "pm_fund_sector")}

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


#### Joins to become views:
# library(tidyverse)
# con = AZASRS_DATABASE_CONNECTION()
# tbl_pm_fund_info(con) %>%
#   left_join(tbl_pm_fund_category(con) , by = 'pm_fund_category_id') %>%
#   left_join(tbl_pm_fund_city(con) , by = 'pm_fund_city_id') %>%
#   left_join(tbl_pm_fund_portfolio(con) , by = 'pm_fund_portfolio_id') %>%
#   left_join(tbl_pm_fund_sector(con) , by = 'pm_fund_sector_id') %>%
#   left_join(tbl_pm_fund_sponsor(con) , by = 'pm_fund_sponsor_id') %>%
#   select(-pm_fund_category_id, pm_fund_city_id, pm_fund_portfolio_id, pm_fund_sector_id, pm_fund_sponsor_id) %>%
#   show_query()
#
