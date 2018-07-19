#' @export
interpolateDays365 = function(zoo_object){
  first_date = -5 + as.Date(min(zoo::index(zoo_object)))
  last_date = as.Date(max(zoo::index(zoo_object)))
  all_dates_between = zoo::zoo(,seq.Date(from = first_date, to = last_date, 'days'))
  days365 = zoo::zooreg(rep(0 , 1 + last_date - first_date),
                  start = first_date, end = last_date)
  zooreg_object = zoo::zoo(zoo_object)
  z_final = zoo::na.approx(merge(days365,zooreg_object)[,2],na.rm=FALSE)
  return(z_final)
}
