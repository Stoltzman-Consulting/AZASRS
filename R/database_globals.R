#' @import RSQLite


# Global variables
AZASRS_DATABASE_LOCATION = "P:\\IMD\\2018 Database Project\\asrs.db"
AZASRS_DATABASE_DRIVER = RSQLite::SQLite()
dbCon = dplyr::src_sqlite(AZASRS_DATABASE_LOCATION)


# All existing tables
a_i = function(con){dplyr::tbl(con, "account_info")}
s_c = function(con){dplyr::tbl(con, "ssbt_composite")}
s_c_i = function(con){dplyr::tbl(con, "ssbt_composite_info")}
p_f_i = function(con){dplyr::tbl(con, "pm_fund_info")}
s_p = function(con){dplyr::tbl(con, "sub_portfolio")}
a_c = function(con){dplyr::tbl(con, "asset_class")}
ca = function(con){dplyr::tbl(con, "category")}
po = function(con){dplyr::tbl(con, "portfolio")}
b_i = function(con){dplyr::tbl(con, "benchmark_info")}
b_m = function(con){dplyr::tbl(con, "benchmark_monthly")}
b_d = function(con){dplyr::tbl(con, "benchmark_daily")}
p_c_q = function(con){dplyr::tbl(con, "pm_cashflow_quarterly")}
p_g_o_q = function(con){dplyr::tbl(con, "pm_gp_official_quarterly")}
b_o_r_m = function(con){dplyr::tbl(con, "book_of_record_monthly")}
b_o_r_d = function(con){dplyr::tbl(con, "book_of_record_daily")}
