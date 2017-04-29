#' Slope of the saturation vapor pressure curve
#'
#' Estimates the slope of the saturation vapor pressure curve, given a specific
#' temperature and saturation vapor pressure curve.
#'
#' Slope is estimated via (Allen et al. 1998):
#'
#' \deqn{\Delta = \frac{4098 e_{s}}{(T + 237.3)^{2}}}
#'
#' Allen RG, Pereira LS, Raes D, Smith M. 1998. Crop evapotranspiration -
#' Guidelines for computing crop water requirements - FAO Irrigation and
#' drainage paper 56. FAO, Rome, 300(9), D05109.
#'
#' @param temperature The air temperature above a body of water (C). \eqn{T}
#' @param es Saturation vapor pressure of air (kPa). \eqn{e_{s}}
#'
#' @return Returns the slope of the saturation vapor curve (\eqn{kPa C^{-1}}).
#' @export
#'
#' @examples
#'
slp_sat_vap <- function(temperature, es){
  eq.top <- 4098 * es
  eq.bottom <- (temperature + 237.3) ^ 2
  Delta <- eq.top / eq.bottom
  Delta
}
