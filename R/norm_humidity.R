#' Normalized humidity
#'
#' \code{norm_humidity} returns the normalized humidity, \eqn{h_{N}} [\eqn{-}],
#' given the saturation vapor pressure at the surface of a lake and atmospheric
#' vapor pressure.
#'
#' Normalized humidity is defined by (Horita et al. 2008):
#'
#' \deqn{h_{N} = (e_{s_{Ts}} - e_{a}) / e_{s_{Ts}}}
#'
#' See Horita et al. (2008) for a discussion concerning the use of relative vs
#' normalized humidity in the Craig-Gordon model. Also note that this
#' formulation of normalized humidity assumes the activity coefficient of water
#' is 1, which is reasonable for most freshwater systems.
#'
#' Horita J, K Rozanski, S Cohen. 2008. Isotope effects in the evaporation of
#' water: A status report of the Craig-Gordon model. Isotopes in environmental
#' and health studies 44 (1): 23–49. DOI: 10.1080/10256010801887174.
#'
#' @param e_s_Ts Saturation vapor pressure at the surface of a lake,
#'   \eqn{e_{s_{Ts}}} (\eqn{kPa}).
#' @param e_a Vapor pressure of the atmosphere, \eqn{e_{a}} (\eqn{kPa}).
#'
#' @export
#'
#' @examples
#'
norm_humidity <- function(e_s_Ts, e_a){
  h_N <- (e_s_Ts - e_a) / e_s_Ts
  h_N
}