#' Get all pm_fund_info RAW
#'
#' @description A view to get all private market fund info, can be filtered. By default, SQL --> SELECT * FROM all_pm_fund_info;
#' @return Returns a tibble or data.frame result with all pm_fund_info metadata.
#' @examples
#' get_pm_fund_info_raw()
#' @export
get_pm_fund_info_raw <- function() {

  dat <- get_url_data("pm_fund_info")

  return(dat)
}

#' Get all pm_fund_info
#'
#' @description A view to get all private market fund info, can be filtered. By default, SQL --> SELECT * FROM all_pm_fund_info;
#' @return Returns a tibble or data.frame result with all pm_fund_info metadata.
#' @examples
#' get_pm_fund_info()
#' @export
get_pm_fund_info <- function(add_benchmark = FALSE) {
  dat <- get_pm_fund_info_raw()

  if (add_benchmark) {
    print("NOTE: Bench addon requires a tibble to be returned, and will override the input argument!")

    dat <- dat %>%
      tibble::as_tibble()

    benchmark_fund_relationship <- get_benchmark_fund_relationship(con = con, get_all_benchmark_types = TRUE,  return_tibble = TRUE) %>%
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
    return(dat)
  }


