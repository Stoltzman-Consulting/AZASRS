library(tidyverse)
library(AZASRS)

my_irrs = function(..., con = AZASRS::AZASRS_DATABASE_CONNECTION(),
                   start_date = '2019-06-30', end_date = get_value_date(con), itd = FALSE){
  build_nav_cash_flow_combined(..., con = con, start_date = start_date, end_date = end_date, itd = itd, return_tibble = TRUE) %>%
    group_by(...) %>%
    summarize(irr = calc_irr(cash_flow = nav_cf, dates = effective_date))
}


my_start = '2016-06-30'
my_end = '2019-06-30'
my_itd = T
portfolio = my_irrs(pm_fund_portfolio, start_date = my_start, end_date = my_end, itd = my_itd)
category = my_irrs(pm_fund_portfolio, pm_fund_category_description, start_date = my_start, end_date = my_end, itd = my_itd)
fund = my_irrs(pm_fund_portfolio, pm_fund_category_description, pm_fund_description, pm_fund_id, start_date = my_start, end_date = my_end, itd = my_itd)
