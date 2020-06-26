#' Get all pm_fund_info
#'
#' @description A view to get all private market fund info, can be filtered. By default, SQL --> SELECT * FROM all_pm_fund_info;
#' @param ... filtering expressions. i.e. pm_fund_portfolio == 'Credit'
#' @param bench_addon appends multiple benchmarks as their own columns. i.e. PVT_Benchmark, SAA_Benchmark, etc.
#' @param return_tibble when TRUE it returns a tibble rather than a SQL call
#' @return Returns a tibble or SQL result with all pm_fund_info metadata.
#' @examples
#' get_pm_fund_info(pm_fund_portfolio == "Credit")
#' # A tibble: 37 x 28
#' # pm_fund_info_id pm_fund_id pm_fund_description  pm_fund_portfol~ pm_fund_sponsor~ pm_fund_city_id pm_fund_sector_~
#' # <int> <chr>      <chr>                           <int>            <int>           <int>            <int>
#' # 1               1 Fund1    FundDesc1                1                1               1                1
#' # 2               2 Fund2    FundDesc2                1                1               1                1
#' # 3               3 Fund3    FundDesc3                1                1               1                1
#' # ... with 27 more rows, and 21 more variables: vintage <int>, commit <int>, unfunded <dbl>, legacy <chr>,
#' #   specialist <chr>, invest_end <chr>, term_end <chr>, extension <dbl>, ext_time <dbl>, ext_used <dbl>,
#' #   fee_cat <chr>, consultant <chr>, adv_board <int>, obsvr <int>, fund_size_m <dbl>, pm_fund_category <chr>,
#' #   pm_fund_category_description <chr>, pm_fund_city <chr>, pm_fund_portfolio <chr>, pm_fund_sector <chr>,
#' #   pm_fund_sponsor <chr>
#' @export
get_pm_fund_info <- function(con = AZASRS_DATABASE_CONNECTION(), add_benchmark = FALSE, return_tibble = TRUE) {
  dat <- tbl_view_all_pm_fund_info(con = con)

  if (add_benchmark) {
    print("NOTE: Bench addon requires a tibble to be returned, and will override the input argument!")

    dat <- dat %>%
      tibble::as_tibble()

    benchmark_fund_relationship <- get_benchmark_fund_relationship(con = con, return_tibble = TRUE) %>%
      tibble::as_tibble() %>%
      tidyr::pivot_wider(names_from = benchmark_type, values_from = benchmark_info_id)

    benchmark_info <- tbl_benchmark_info(con = con) %>%
      dplyr::select(benchmark_info_id, benchmark_id) %>%
      tibble::as_tibble()

    benchmark_joined <- benchmark_fund_relationship %>%
      dplyr::left_join(benchmark_info, by = c("PVT" = "benchmark_info_id")) %>%
      dplyr::left_join(benchmark_info, by = c("SAA" = "benchmark_info_id")) %>%
      dplyr::left_join(benchmark_info, by = c("IMP" = "benchmark_info_id")) %>%
      dplyr::left_join(benchmark_info, by = c("Sector" = "benchmark_info_id")) %>%
      dplyr::rename(PVT_Benchmark = benchmark_id.x, SAA_Benchmark = benchmark_id.y, IMP_Benchmark = benchmark_id.x.x, Sector_Benchmark = benchmark_id.y.y) %>%
      dplyr::select(-PVT, -SAA, -IMP, -Sector)

    dat <- dat %>%
      dplyr::left_join(benchmark_joined, by = "pm_fund_info_id")
    return(dat)
  }

  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
