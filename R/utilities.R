
#' @export
tibble_to_zoo_list = function(tibb, omit_na = FALSE){
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
