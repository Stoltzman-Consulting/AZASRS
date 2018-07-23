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


#' For the express use within get_variables functions
#' @export
modify_365_zoo = function(data_frame, return_zoo, convert_365){

  dat = data_frame

  if(return_zoo == TRUE){ ### relies on price and date columns (names important)
    dat = zoo::zoo(dat$price, as.Date(dat$date))
  }

  if(convert_365 == TRUE){ ### relies on price and date columns (names important)
    dat_z = zoo::zoo(dat$price, as.Date(dat$date))
    dat_z_365 = interpolateDays365(dat_z)
    dat = dat_z_365
  }

  return(dat)

}
