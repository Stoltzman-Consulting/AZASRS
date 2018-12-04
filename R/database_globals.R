
# Global variables
#' @export
#AZASRS_DATABASE_LOCATION = "C:\\Users\\scotts\\Documents\\GitHub\\ASRS-Application\\database\\asrs.db"
AZASRS_DATABASE_LOCATION = "P:\\IMD\\2018 Database Project\\asrs_new.db"

#' @export
AZASRS_DATABASE_DRIVER = RSQLite::SQLite()

#' @export
AZASRS_DATABASE_CONNECTION = function(){ return(dplyr::src_sqlite(AZASRS_DATABASE_LOCATION)) }


# All existing tables

#' @export
tbl_account_info = function(con){dplyr::tbl(con, "account_info")}

#' @export
tbl_ssbt_composite = function(con){dplyr::tbl(con, "ssbt_composite")}

#' @export
tbl_ssbt_composite_info = function(con){dplyr::tbl(con, "ssbt_composite_info")}

#' @export
tbl_pm_fund_info = function(con){dplyr::tbl(con, "pm_fund_info")}

#' @export
tbl_sub_portfolio = function(con){dplyr::tbl(con, "sub_portfolio")}

#' @export
tbl_asset_class = function(con){dplyr::tbl(con, "asset_class")}

#' @export
tbl_category = function(con){dplyr::tbl(con, "category")}

#' @export
tbl_portfolio = function(con){dplyr::tbl(con, "portfolio")}

#' @export
tbl_previous_saa = function(con){dplyr::tbl(con, "previous_saa")}

#' @export
tbl_sponsor = function(con){dplyr::tbl(con, "sponsor")}

#' @export
tbl_saa_benchmark = function(con){dplyr::tbl(con, "saa_benchmark")}

#' @export
tbl_imp_benchmark = function(con){dplyr::tbl(con, "imp_benchmark")}

#' @export
tbl_ticker = function(con){dplyr::tbl(con, "ticker")}

#' @export
tbl_city = function(con){dplyr::tbl(con, "city")}

#' @export
tbl_benchmark_info = function(con){dplyr::tbl(con, "benchmark_info")}

#' @export
tbl_benchmark_monthly = function(con){dplyr::tbl(con, "benchmark_monthly")}

#' @export
tbl_benchmark_daily = function(con){dplyr::tbl(con, "benchmark_daily")}

#' @export
tbl_pm_cashflow_quarterly = function(con){dplyr::tbl(con, "pm_cashflow_quarterly")}

#' @export
tbl_pm_gg_official_quarterly = function(con){dplyr::tbl(con, "pm_gp_official_quarterly")}

#' @export
tbl_book_of_record_monthly = function(con){dplyr::tbl(con, "book_of_record_monthly")}

#' @export
tbl_book_of_record_daily = function(con){dplyr::tbl(con, "book_of_record_daily")}
