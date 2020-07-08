#' Build ALL PM metrics for ALL dates (massive calculation, be prepared)
#'
#' @description Builds a tibble for ALL calculations for every start/end date combination up until valdate
#' @return Returns a large tibble
#' @export
build_all_grouped_pm_metrics_all_dates = function(){

  con = AZASRS_DATABASE_CONNECTION()
  end_date = get_value_date(con)
  nav_daily = get_pm_nav_daily(con = con)
  cf_daily = get_pm_cash_flow_daily(con = con)
  bench_daily = get_benchmark_daily_index(con = con, benchmark_type = "PVT", return_tibble = TRUE)
  bench_relationships = get_benchmark_fund_relationship(con = con, bench_type = "PVT", return_tibble = TRUE)
  pm_fund_info = get_pm_fund_info(con = con)

  date_ranges <- tibble()
  for (i in c(1, 4, 12, 20, 40)) {
    date_range <- build_lagged_date_range_df(
      start_date = "2004-06-30",
      end_date = end_date, n_qtrs = i
    ) %>%
      mutate(n_qtrs = i)
    date_ranges <- bind_rows(date_ranges, date_range)
  }

  calc_all = function(...){
    metrics = tibble()
    for(dr in 1:nrow(date_ranges)){
      tmp = date_ranges[dr,]
      tmp_data = build_grouped_pm_metrics(...,
                                          con = con,
                                          start_date = tmp$start_date,
                                          end_date = tmp$end_date,
                                          itd = FALSE,
                                          cash_adjusted = FALSE,
                                          nav_daily = nav_daily,
                                          cf_daily = cf_daily,
                                          bench_daily = bench_daily,
                                          bench_relationships = bench_relationships,
                                          pm_fund_info = pm_fund_info)

      metrics = bind_rows(metrics, tmp_data)
    }

    tmp_data = build_grouped_pm_metrics(...,
                                        con = con,
                                        start_date = tmp$start_date,
                                        end_date = tmp$end_date,
                                        itd = TRUE,
                                        cash_adjusted = FALSE,
                                        nav_daily = nav_daily,
                                        cf_daily = cf_daily,
                                        bench_daily = bench_daily,
                                        bench_relationships = bench_relationships,
                                        pm_fund_info = pm_fund_info)

    metrics = bind_rows(metrics, tmp_data)

    return(metrics)
  }

  fund = calc_all(pm_fund_common_name, pm_fund_description, pm_fund_id)
  portfolio = calc_all(pm_fund_portfolio)
  category = calc_all(pm_fund_portfolio, pm_fund_category_description)
  category_fund = calc_all(pm_fund_portfolio, pm_fund_category_description, pm_fund_common_name, pm_fund_description, pm_fund_id)
  sponsor = calc_all(pm_fund_portfolio, pm_fund_sponsor)
  sponsor_fund = calc_all(pm_fund_portfolio, pm_fund_sponsor, pm_fund_common_name, pm_fund_description, pm_fund_id)

  all_data = bind_rows(fund, portfolio, category, category_fund, sponsor, sponsor_fund)

  return(all_data)

}
