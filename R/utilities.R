
#' @export
tibble_to_zoo_list = function(tibb, omit_na = TRUE){
  listify = function(x){
    ts_list = list()
    columns = colnames(x)
    for(i in 2:ncol(x)){
      if(omit_na == TRUE){
        ts_list[[columns[i]]] = x[,i] %>% na.omit()
      }
      else{
        ts_list[[columns[i]]] = x[,i]
      }
    }
    return(ts_list)
  }
  dat = tibb %>%
    tsbox::ts_zoo() %>%
    listify()
  return(dat)
}

# Creates a csv from every single table in db
# library(tidyverse)
# library(AZASRS)
# con = AZASRS_DATABASE_CONNECTION()
# tbl_names = dplyr::src_tbls(con)
#
# for(tbl_name in tbl_names){
#   print(tbl_name)
#   if(tbl_name %in% c('all_account_info', 'all_pm_fund_info', 'all_benchmark_daily')){next}
#   dat = dplyr::tbl(con, tbl_name) %>% as_tibble()
#   write_csv(dat, paste0('data_tables/',tbl_name,'.csv'))
# }
