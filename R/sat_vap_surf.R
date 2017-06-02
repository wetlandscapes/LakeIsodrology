#' Saturation vapor pressure at the water surface
#'
#' \code{sat_vap_surf} returns the estimated saturation vapor pressure,
#' \eqn{e_{ls}}, of a water body at its surface in \eqn{kPa}.
#'
#' The saturation vapor pressure of a water body is determined given a known air
#' temperature, lake surface temperature, the slope of the saturation vapor
#' pressure curve for a given temperature and saturation vapor pressure for the
#' atmosphere. The relationship used is (Dingman 2008):
#'
#' \deqn{e_{ls} = e_{s} + \Delta(T_{ls} - T_{a})}
#'
#' Dingman SL. 2008. Physical hydrology. Waveland Press.
#'
#' @param es The saturation vapor pressure of the atmosphere,
#'   \eqn{e_{s}} (\eqn{kPa}).
#' @param del Slope of the saturation vapor pressure curve, \eqn{\Delta}
#'   (\eqn{kPa C^{-1}}).
#' @param Tls Water temperature at the lake surface, \eqn{T_{ls}} (\eqn{C}).
#' @param Ta Temperature of the atmosphere, \eqn{T_{a}} (\eqn{C}).
#'
#' @export
#'
#' @examples
#'
sat_vap_surf <- function(es, del, Tls, Ta){
  els <- es + (del * (Tls - Ta))
  els
}
