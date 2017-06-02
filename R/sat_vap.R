#' Saturation vapor pressure of air
#'
#' \code{sat_vap} determines the saturation vapor pressure, \eqn{e_{s}} (\eqn{kPa}), for a
#' given atmospheric temperature.
#'
#' Saturation vapor pressure is determine by (Allen et al. 1998):
#'
#' \deqn{e_{s} = \frac{0.611 e^{(T_{a} 17.3)}}{T_{a} + 237.3}}
#'
#' Allen, RG, LS Pereira, D Raes, M Smith. 1998. Crop evapotranspiration -
#' Guidelines for computing crop water requirements - FAO Irrigation and
#' drainage paper 56. FAO, Rome, 300(9), D05109.
#'
#' @param Ta Air temperature, \eqn{T_{a}} (\eqn{C}).
#'
#' @export
#'
#' @examples
#'
sat_vap <- function(Ta){
  right <- exp((Ta * 17.3) / (Ta + 237.3))
  es <- 0.611 * right
  es
}
