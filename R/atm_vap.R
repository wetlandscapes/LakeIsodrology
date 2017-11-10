#' Atmospheric vapor pressure
#'
#' \code{atm_vap} returns the ambient atmospheric vapor pressure, \eqn{e_{a}}
#' (\eqn{kPa}), given a relative humidity and saturation vapor pressure for a
#' particular temperature.
#'
#' Atmospheric vapor pressure (Dingman 2008):
#'
#' \deqn{e_{a} = W \cdot e_{s}}
#'
#' Dingman SL. 2008. Physical hydrology. Waveland Press.
#'
#' @param W Relative humidity, \eqn{W} (\eqn{%}).
#' @param es Saturation vapor pressure, \eqn{e_s} (\eqn{kPa}).
#'
#' @export
#'
#' @examples
#'
atm_vap <- function(W, es){
  ea <- (W / 100) * es
  ea
}
