#' Saturation vapor density
#'
#' \code{sat_vap_dens} estimates the saturation vapor density, \eqn{\rho_{s}}, returning
#' a value in \eqn{g m^{-3}}.
#'
#' \eqn{\rho_{s}} is estimated by (Manton and Cotton 1977):
#'
#' \deqn{\rho_{s} = \frac{e_{s}}{R \cdot T_{a}}}
#'
#' Note that values for \eqn{e_{s}}, \eqn{T_{a}}, and \eqn{\rho_{s}} are
#' converted to unit consistent values within the function.
#'
#' Manton M, Cotton W. 1977. Formulation of approximate equations for modeling
#' moist deep convection on the mesoscale. Atmospheric science paper; no. 266.
#' Colorado State University, Fort Collins, CO.
#'
#' @param es Saturation vapor pressure, \eqn{e_{s}} (\eqn{kPa})
#' @param Ta Air temperature, \eqn{T_{a}} (\eqn{C}).
#' @param R Gas constant for water vapor, \eqn{\rho_{s}} (\eqn{J kg^{-1}
#'   K^{-1}}). Assumed to be 461.5.
#'
#' @export
#'
#' @examples
#'
sat_vap_dens <- function(es, Ta, R = 461.5){
  #Convert kPa to Pa.
  es.Pa <- es * 1000
  #Convert C to K.
  Ta.K <- Ta + 273.15
  #Convert J/(kg K) to J/(g K).
  R.g <- R / 1000
  ps <- es.Pa / (R.g * Ta.K)
  ps
}
