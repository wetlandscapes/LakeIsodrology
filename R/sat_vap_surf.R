#' Saturation vapor pressure at the water surface
#'
#' \code{sat_vap_surf} returns the estimated saturation vapor pressure,
#' \eqn{e_{s_{Ts}}}, of a water body at its surface in \eqn{kPa}.
#'
#' The saturation vapor pressure of a water body is determined given a known air
#' temperature, lake surface temperature, the slope of the saturation vapor
#' pressure curve for a given temperature and saturation vapor pressure for the
#' atmosphere. The relationship used is (Dingman 2008):
#'
#' \deqn{e_{s_{Ts}} = e_{s_{Ta}} + \Delta(T_{s} - T_{a})}
#'
#' Dingman SL. 2008. Physical hydrology. Waveland Press.
#'
#' @param e_s_Ta The saturation vapor pressure of the atmosphere,
#'   \eqn{e_{s_{Ta}}} (\eqn{kPa}).
#' @param Del Slope of the saturation vapor pressure curve, \eqn{\Delta}
#'   (\eqn{kPa C^{-1}}).
#' @param Ts Water temperature at the lake surface, \eqn{Ts} (\eqn{C}).
#' @param Ta Temperature of the atmosphere, \eqn{Ta} (\eqn{C}).
#'
#' @export
#'
#' @examples
#'
sat_vap_surf <- function(e_s_Ta, Del, Ts, Ta){
  e_s_Ts <- e_s_Ta + (Del * (Ts - Ta))
  s_s_Ts
}
