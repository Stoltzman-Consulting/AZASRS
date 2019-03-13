context('Test function get_account_info')

get_test_data = function(filename){
  input_location = paste0(AZASRS::AZASRS_TEST_DATA_DIRECTORY, filename, ".rda")
  inputs = load(file = input_location)
  return(get(inputs))
}

test_that("Recon - daily.data is the same as database", {
  recon.daily.data = get_test_data('daily.data') %>%
    select(Date, ID, b.mv, e.mv, cf, r.day, b.day) %>%
    dplyr::arrange(Date,ID)

  daily.data.account = daily_data_account()
  daily.data.composite = daily_data_composite()
  daily.data = daily.data.account %>% rename(daily_return = account_daily_return) %>%
    dplyr::bind_rows(daily.data.composite %>%
                dplyr::rename(daily_return = composite_daily_return)) %>%
    dplyr::filter(effective_date >= fytd.bgn, effective_date <= fytd.end) %>%
    dplyr::rename(Date = effective_date,
           b.mv = beginning_market_value,
           e.mv = ending_market_value,
           cf = net_cash_flow,
           r.day = daily_return,
           b.day = benchmark_daily_return) %>%
    dplyr::select(Date, Name, ID, b.mv, e.mv, cf, r.day, b.day) %>%
    dplyr::mutate(Date = ymd(Date, tz = "America/Los_Angeles")) %>%
    select(Date, ID, b.mv, e.mv, cf, r.day, b.day) %>%
    drop_na() %>%
    dplyr::arrange(Date,ID)

  #TODO: Get checks from ASRS Team
  expect_equal(colnames(daily.data), colnames(recon.daily.data)) # Column names
  expect_equal(sapply(daily.data, class), sapply(recon.daily.data, class)) # Column data types
  expect_equal(unique(recon.daily.data$Date), unique(daily.data$Date))
  expect_equal(unique(recon.daily.data$ID), unique(daily.data$ID))
  expect_equal(sum(recon.daily.data$b.mv), sum(daily.data$b.mv))
  expect_equal(sum(recon.daily.data$e.mv), sum(daily.data$e.mv))
  expect_equal(sum(recon.daily.data$cf), sum(daily.data$cf))
  expect_equal(sum(recon.daily.data$b.day), sum(daily.data$b.day))
  expect_equal(sum(recon.daily.data$r.day), sum(daily.data$r.day))
})


test_that("Recon - monthly.data is the same as database", {
  recon.monthly.data = get_test_data('monthly.data') %>%
    select(Date, ID, r.MTD, b.MTD, r.FYTD, b.FYTD) %>%
    dplyr::arrange(Date,ID)

  monthly.data.account = monthly_data_account()
  monthly.data.composite = monthly_data_composite()
  monthly.data = monthly.data.account %>%
    rename(monthly_return = account_monthly_return,
           fiscal_ytd_return = account_fiscal_ytd_return) %>%
    bind_rows(monthly.data.composite %>%
                rename(monthly_return = composite_monthly_return,
                       fiscal_ytd_return = composite_fiscal_ytd_return)) %>%
    filter(effective_date >= fytd.bgn, effective_date <= fytd.end) %>%
    rename(Date = effective_date,
           r.MTD = monthly_return,
           b.MTD = benchmark_monthly_return,
           r.FYTD = fiscal_ytd_return,
           b.FYTD = benchmark_fiscal_ytd_return) %>%
    select(Date, ID, r.MTD, b.MTD, r.FYTD, b.FYTD) %>%
    mutate(Date = ymd(Date, tz = "America/Los_Angeles")) %>%
    dplyr::arrange(Date,ID)

  #TODO: Get checks from ASRS Team
  expect_equal(colnames(monthly.data), colnames(recon.monthly.data)) # Column names
  expect_equal(sapply(monthly.data, class), sapply(recon.monthly.data, class)) # Column data types
  expect_equal(unique(recon.monthly.data$Date), unique(monthly.data$Date))
  expect_equal(unique(recon.monthly.data$ID), unique(monthly.data$ID))
  expect_equal(sum(recon.monthly.data$r.MTD), sum(monthly.data$r.MTD))
  expect_equal(sum(recon.monthly.data$b.MTD), sum(monthly.data$b.MTD))
  expect_equal(sum(recon.monthly.data$r.FYTD), sum(monthly.data$r.FYTD))
  expect_equal(sum(recon.monthly.data$b.FYTD), sum(monthly.data$b.FYTD))
})

test_that("Database returns proper column names and data types", {
  account_info_columns = sapply(get_account_info(), class)
  expect_equal(account_info_columns, test_account_info_columns)
  # to create test data
  # test_account_info_columns = account_info_columns
  # devtools::use_data(test_account_info_columns)
})

test_that("Database returns data filtered by multiple columns", {
  account_info_filtered = get_account_info(asset_class == 'Equities', category == 'Large')
  expect_equal(account_info_filtered, test_account_info_filtered)
})
