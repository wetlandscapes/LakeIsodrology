#' Psychrometric constant
#'
#' Determines the psychrometric constant, \eqn{\gamma} (\eqn{kPa K^{-1}}), at a
#' given pressure.
#'
#' \eqn{\gamma} is defined as (Dingman 2008):
#'
#' \deqn{\gamma = \frac{c_{a} P}{0.622 \lambda_{v}}}
#'
#' Dingman SL. 2008. Physical hydrology. Waveland Press.
#'
#' @param P Air pressure, \eqn{P} (\eqn{kPa}). Assumed to be 101.3, a common
#'   value at sea level.
#' @param ca Heat capacity of air, \eqn{c_{a}} (\eqn{MJ kg^{-1} K^{-1}}).
#'   Assumed to be 1e-3.
#' @param L Latent heat of vaporization for water, \eqn{\lambda_{v}}
#'   (\eqn{MJ kg^{-1}}). Assumed to be 2.47.
#'
#' @export
#'
#' @examples
#'
psychrometric <- function(P = 101.3, ca = 1e-3, L = 2.47){
  top <- ca * P
  bot <- 0.622 * latent
  gamma <- top / bottom
  gamma
}
