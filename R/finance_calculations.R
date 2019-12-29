# File is reserved for basic financial calculations that should be standardized across ASRS

#' IRR
#'
#' @description Calculates IRR based off of asrsMethods library. Can be used with tidyverse style.
#' @param cash_flow is a c() of cash_flows with the first being the earliest NAV (negative) and the last being NAV (positive)
#' @param dates is a c() of dates that corresponds with the cash_flow
#' @export
calc_irr = function(cash_flow, dates){
  dat <- asrsMethods::irr.z(zoo::zoo(cash_flow, dates), gips = TRUE)
  return(dat)
}


#' TVPI
#'
#' @description Calculates TVPI that is NOT based off of asrsMethods library. asrsMethods needs to be updated because it does not reflect TVPI as Kerry calculates.
#' @param distributions are negative cash flow
#' @param contributions are positive cash flow
#' @param nav is NAV at the beginning and end with zeros in between. Both NAV are positive.
#' @export
calc_tvpi = function(distributions, contributions, nav){

  if(sum(contributions == 0)){
    contributions[1] = -1*abs(nav[1])
  }
  if(sum(distributions == 0)){
    distributions[1] = abs(nav[2])
  }

  dat = sum(distributions + nav) / sum(abs(contributions))
  return(dat)
}


#' DPI
#'
#' @description Calculates DPI that is NOT based off of asrsMethods library. asrsMethods needs to be updated because it does not reflect TVPI as Kerry calculates.
#' @param distributions are negative cash flow
#' @param contributions are positive cash flow
#' @export
calc_dpi = function(distributions, contributions, nav){

  if(sum(contributions == 0)){
    contributions[1] = -1*abs(nav[1])
  }
  if(sum(distributions == 0)){
    distributions[1] = abs(nav[2])
  }

  dat = sum(abs(distributions)) / sum(abs(contributions))
  return(dat)
}

#' Appreciation
#'
#' @description Calculates Appreciation that is NOT based off of asrsMethods library. asrsMethods needs to be updated because it does not reflect TVPI as Kerry calculates.
#' @param cash_flow is a c() of cash flows
#' @param nav is NAV at the beginning and end with zeros in between. Both NAV are positive.
#' @export
calc_appreciation = function(cash_flow, nav){
  dat = sum(nav) + sum(cash_flow)
  return(dat)
}

#' PME
#'
#' @description Calculates PME that is NOT based off of asrsMethods library. asrsMethods needs to be updated because it does not reflect TVPI as Kerry calculates.
#' @param distributions are negative cash flow
#' @param contributions are positive cash flow
#' @param nav is NAV at the beginning and end with zeros in between. Both NAV are positive.
#' @param fv_index_factors are a c() of future value data points based off of index value divided by first index value (i.e. 1.18, 1.11, 1.05, 1.00)
#' @export
calc_pme = function(distributions, contributions, nav, fv_index_factors){

  if(sum(contributions == 0)){
    contributions[1] = -1*abs(nav[1])
  }
  if(sum(distributions == 0)){
    distributions[1] = abs(nav[2])
  }

  total_fv_distributions = sum((distributions * fv_index_factors) + nav)
  total_fv_contributions = abs(sum(contributions * fv_index_factors))
  pme = total_fv_distributions / total_fv_contributions
  return(pme)
}


#' DVA
#'
#' @description Calculates DVA that is NOT based off of asrsMethods library.
#' @param cash_flow is the cash flow tibble
#' @param fv_index_factors are a c() of future value data points based off of index value divided by first index value (i.e. 1.18, 1.11, 1.05, 1.00)
#' @export
calc_dva = function(cash_flow, fv_index_factors){

  dva = sum(cash_flow * fv_index_factors)

  return(dva)
}


