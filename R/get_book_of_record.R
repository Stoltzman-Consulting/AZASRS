
#' Query book_of_record_..._ table from database
#'
#' @param record_type is either 'composite' or 'account'
#' @param frequency is either 'daily' or 'monthly'
#' @param ... are for filtering the tables, can take values: category, portfolio, asset_class, sub_portfolio, previous_saa, sponsor, saa_benchmark, imp_benchmark, ticker, city
#' @return Returns a tibble of all of the data
#' @examples
#' get_book_of_record_daily('account', 'daily', asset_class == 'Equities')
#' @export
get_book_of_record = function(record_type, frequency, ..., con = AZASRS_DATABASE_CONNECTION()){

  args = rlang::enexprs(...)

  if(record_type == 'composite'){
    if(frequency == 'daily'){
      dat = tbl_book_of_record_composite_daily(con)
    }
    else if(frequency == 'monthly'){
      dat = tbl_book_of_record_composite_monthly(con)
    }

    if(length(args) > 0){
      ssbt_i = get_account_info(..., con = con)
      dat = ai %>%
        dplyr::left_join(dat, by = c('ssbt_composite_info_id' = 'ssbt_composite_info_id'), copy = TRUE)
    }

  }

  if(record_type == 'account'){
    if(frequency == 'daily'){
      dat = tbl_book_of_record_account_daily(con)
    }
    else if(frequency == 'monthly'){
      dat = tbl_book_of_record_account_monthly(con)
    }

    ai = get_account_info(...)
    dat = ai %>%
      dplyr::left_join(dat, by = c('account_info_id' = 'account_info_id'), copy = TRUE)

  }

  return(dat %>% tibble::as_tibble())
}
