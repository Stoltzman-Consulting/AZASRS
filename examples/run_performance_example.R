library('AZASRS')
library('tidyverse')
library('lubridate')

ending_date = AZASRS::get_value_date()
years_quarters = tibble(years    = c(0, 1, 3, 5, 10, 115),
                        quarters  = c(1, 0, 0, 0, 0, 0),
                        cool_name = factor(c('Quarter',
                                             '1 Year',
                                             '3 Year',
                                             '5 Year',
                                             '10 Year',
                                             'ITD'),
                                           ordered = TRUE,
                                           levels = c('Quarter', '1 Year', '3 Year', '5 Year', '10 Year', 'ITD'))) %>%
  mutate(end_date = ending_date,
         start_date = calc_previous_year_qtr(ending_date, years, quarters))


final_data = list()
for(i in 1:nrow(years_quarters)){
  yq = years_quarters[i,]
  final_data[[i]] = build_privm_p2p_irr(yq$start_date, yq$end_date)
  final_data[[i]]$start_date = yq$start_date
  final_data[[i]]$end_date = yq$end_date
  final_data[[i]]$cool_name = yq$cool_name
}
all_irrs = bind_rows(final_data)


all_navs = get_pm_nav_daily()
privm_metrics = build_privm_metrics(pm_fund_portfolio, pm_fund_category_description)
pmfi = get_pm_fund_info()

portfolios = privm_metrics$pm_fund_portfolio %>% unique()
param_list = map(portfolios,
                 ~list(portfolio_filter = .,
                       all_navs = all_navs, privm_metrics = privm_metrics, all_irrs = all_irrs, pmfi = pmfi))

reports <- tibble(
  output_file = stringr::str_c("examples/output/", portfolios, "-performance_report.html"),
  params = param_list)

reports %>%
  purrr::pwalk(rmarkdown::render, input = 'examples/Performance Report Example.Rmd')
