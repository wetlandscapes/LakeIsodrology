#' Saturation vapor pressure of air
#'
#' \code{sat_vap} determines the saturation vapor pressure, \eqn{e_{s}} (\eqn{kPa}), for a
#' given atmospheric temperature.
#'
#' Saturation vapor pressure is determine by (Allen et al. 1998):
#'
#' \deqn{e_{s} = \frac{0.611 exp(T 17.3)}{T + 237.3}}
#'
#' Allen, RG, LS Pereira, D Raes, M Smith. 1998. Crop evapotranspiration -
#' Guidelines for computing crop water requirements - FAO Irrigation and
#' drainage paper 56. FAO, Rome, 300(9), D05109.
#'
#' @param temperature Air temperature \eqn{T} (\eqn{C})
#'
#' @export
#'
#' @examples
#'
sat_vap <- function(temperature){
  right <- exp((T * 17.3) / (T + 237.3))
  es <- 0.611 * right
  es
}
