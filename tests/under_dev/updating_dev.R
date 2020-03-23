library(tidyverse)
library(AZASRS)
library(testthat)

con_dev = AZASRS_DATABASE_CONNECTION(development = 1)
con_prod = AZASRS_DATABASE_CONNECTION(development = 0)

pmfi_dev = get_pm_fund_info(con = con_dev)
pmfi_prod = get_pm_fund_info(con = con_prod)
# Added common name
expect_equal(colnames(pmfi_dev), colnames(pmfi_prod))




###
dev = get_pm_nav_daily(con = con_dev) %>%
  select(-pm_fund_common_name, -extension, -ext_time, -ext_used, -fund_size_m, -commit, -unfunded) %>%
  arrange(pm_fund_info_id, effective_date)

prod = get_pm_nav_daily(con = con_prod) %>%
  select(-extension, -ext_time, -ext_used, -fund_size_m, -commit, -unfunded) %>%
  arrange(pm_fund_info_id, effective_date)

expect_equal(dev, prod)
###


###
dev = get_pm_cash_flow_daily(con = con_dev) %>%
  select(-extension, -ext_time, -ext_used, -fund_size_m, -commit, -unfunded) %>%
  arrange(pm_fund_info_id, effective_date)

prod = get_pm_cash_flow_daily(con = con_prod) %>%
  select(-extension, -ext_time, -ext_used, -fund_size_m, -commit, -unfunded) %>%
  arrange(pm_fund_info_id, effective_date)

expect_equal(dev, prod)
###



###
dev = get_benchmark_daily_index(con = con_dev) %>% as_tibble()
prod = get_benchmark_daily_index(con = con_prod) %>% as_tibble()

expect_equal(dev, prod)
###
