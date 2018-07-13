#' Trailing Period IRR
#'
#' Calculates trailing period IRR
#'
#' @param db_con database connection
#' @param shortname name of total portfolio from nav and cashflow tables
#' @param portfolio_name name of portfolio from fundinfo table
#' @param comparison_years trailing years, default is c(1,3,5)
#' @param benchmark_type from fundinfo table: benchmark, stype, sector
#' @param benchmark_symbol benchmark to compare against - this overrides benchmark_type
#' @return dataframe of years/dates, IRR benchmark, IRR priv
#' @export
trailing_period_irr = function(shortname = 'Total PE',
                               portfolio_name = 'PE',
                               comparison_years = c(1,3,5),
                               benchmark_type = 'benchmark',
                               benchmark_symbol = 'DEFAULT_FROM_FUNDINFO'){

  # Database queries
  valdate = get_valdate()
  prev_qtr = lubridate::as_date(zoo::as.yearqtr(valdate)) - 1
  cf = get_filtered_cashflow(shortname) %>% dplyr::mutate(date = as.Date(date))
  val = get_filtered_nav(shortname) %>% dplyr::mutate(date = as.Date(date))
  fundinfo = pull_fundinfo() %>% tibble::as_tibble()


  # Data preparation
  lookback_years = c(prev_qtr, valdate - lubridate::years(comparison_years))

  if(benchmark_symbol == 'DEFAULT_FROM_FUNDINFO'){
    benchmark_names = fundinfo %>%
      dplyr::filter(portfolio == portfolio_name) %>%
      dplyr::select_(benchmark_type) %>%
      unique() %>%
      dplyr::rename_('benchmark' = benchmark_type)
    benchmark_name = benchmark_names$benchmark[1]
  } else {
    benchmark_name = benchmark_symbol
  }

  benchmark = get_filtered_benchmark(shortname = benchmark_name) %>%
    tibble::as_tibble()


  # Calculation and manipulation
  cf.combine = cf %>%
    dplyr::filter(date <= valdate) %>%
    dplyr::filter(date >= prev_qtr) %>%
    dplyr::bind_rows(val %>% dplyr::filter(date == as.character(valdate) | date == as.character(prev_qtr))) %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(amount = sum(amount)) %>%
    dplyr::mutate(amount = dplyr::if_else(date == prev_qtr, -1 * amount, amount)) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::arrange(date)

  # Convert to fit Micheal's existing code
  # Create zoo object to utilize pestats function
  cf.combine.z = zoo::zoo(cf.combine$amount, cf.combine$date)
  benchmark.z = na.omit(zoo::zoo(benchmark$price, as.Date(benchmark$date)))
  val.z = zoo::zoo(val$amount, as.Date(val$date))
  cf.z = zoo::zoo(cf$amount, cf$date)

  pe.qtr = pestats(cf.combine.z, benchmark.z[time(cf.combine.z)])

  irr.bench.c = pe.qtr$ind.irr
  irr.priv.c = pe.qtr$irr
  yr.c = as.character(lookback_years[1])

  for(yr in tail(as.character(lookback_years), -1)) {
    yr.c = c(yr.c, yr)
    yr = as.Date(yr)

    cf.yr = cf.z[time(cf.z)<=valdate&time(cf.z)>yr]
    dat = mergesum.z(-1*val.z[yr], cf.yr, val.z[valdate])
    irr.yr <- irr.z(dat, gips = TRUE)
    irr.priv.c <- c(irr.priv.c, irr.yr)

    fvfactor = as.numeric(lastinvec(benchmark.z))/benchmark.z
    cf.fv = dat * fvfactor
    alpha = log(1+irr.z(cf.fv, gips = TRUE))
    logpe.irr = log(1 + irr.yr)
    logdm.irr = logpe.irr - alpha
    irr.bench.c = c(irr.bench.c, -1+exp(logdm.irr))
  }

  irr_df = tibble::tibble(trailing_period = c(0, comparison_years),
                          year = yr.c,
                          bench = irr.bench.c,
                          priv = irr.priv.c)

  return(irr_df)
}
