#' Atmospheric vapor pressure
#'
#' \code{atm_vap} returns the ambient atmospheric vapor pressure, \eqn{e_{a}}
#' (\eqn{kPa}), given a relative humidity and saturation vapor pressure for a
#' particular temperature.
#'
#' Atmospheric vapor pressure (Dingman 2008):
#'
#' \deqn{e_{a} = RH e_{s_{Ta}}}
#'
#' Dingman SL. 2008. Physical hydrology. Waveland Press.
#'
#' @param RH Relative humidity, \eqn{RH} [\eqn{-}]
#' @param e_sat Saturation vapor pressure, \eqn{e_{s_{Ta}}} (\eqn{kPa}).
#'
#' @export
#'
#' @examples
#'
atm_vap <- function(RH, e_sat){
  e_a <- RH * e_sat
  e_a
}
