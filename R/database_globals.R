
# Global variables
#' @export
AZASRS_DATABASE_LOCATION = "C:\\Users\\scotts\\Desktop\\2018 Database Project\\Database\\asrs_database.db"
#AZASRS_DATABASE_LOCATION = "P:\\IMD\\2018 Database Project\\asrs_new.db"

#' @export
AZASRS_DATABASE_DRIVER = RSQLite::SQLite()

#' @export
AZASRS_DATABASE_CONNECTION = function(){ return(dplyr::src_sqlite(AZASRS_DATABASE_LOCATION)) }

# All existing tables

#' @export
tbl_account_info = function(con){df = dplyr::tbl(con, "account_info")}

#' @export
tbl_asset_class = function(con){dplyr::tbl(con, "asset_class")}

#' @export
tbl_benchmark_info = function(con){dplyr::tbl(con, "benchmark_info")}

#' @export
tbl_book_of_record_account_daily = function(con){dplyr::tbl(con, "book_of_record_account_daily")}

#' @export
tbl_book_of_record_account_monthly = function(con){dplyr::tbl(con, "book_of_record_account_monthly")}

#' @export
tbl_book_of_record_composite_daily = function(con){dplyr::tbl(con, "book_of_record_composite_daily")}

#' @export
tbl_book_of_record_composite_monthly = function(con){dplyr::tbl(con, "book_of_record_composite_monthly")}

#' @export
tbl_category = function(con){dplyr::tbl(con, "category")}

#' @export
tbl_city = function(con){dplyr::tbl(con, "city")}

#' @export
tbl_pm_cashflow_daily = function(con){dplyr::tbl(con, "pm_cashflow_daily")}

#' @export
tbl_pm_fund_info = function(con){dplyr::tbl(con, "pm_fund_info")}

#' @export
tbl_pm_nav_daily = function(con){dplyr::tbl(con, "pm_nav_daily")}

#' @export
tbl_portfolio = function(con){dplyr::tbl(con, "portfolio")}

#' @export
tbl_previous_saa = function(con){dplyr::tbl(con, "previous_saa")}

#' @export
tbl_sponsor = function(con){dplyr::tbl(con, "sponsor")}

#' @export
tbl_ssbt_composite_info = function(con){dplyr::tbl(con, "ssbt_composite_info")}

#' @export
tbl_ssbt_composite_info_account_info = function(con){dplyr::tbl(con, "ssbt_composite_info_account_info")}

#' @export
tbl_sub_portfolio = function(con){dplyr::tbl(con, "sub_portfolio")}
