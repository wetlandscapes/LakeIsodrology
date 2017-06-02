#' Normalized humidity
#'
#' \code{norm_humidity} returns the normalized humidity, \eqn{h_{n}} [\eqn{-}],
#' given the saturation vapor pressure at the surface of a lake and atmospheric
#' vapor pressure.
#'
#' Normalized humidity is defined by (Horita et al. 2008):
#'
#' \deqn{h_{n} = \frac{(e_{ls} - e_{a})}{e_{ls}}}
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
#' @param els Saturation vapor pressure at the surface of a lake,
#'   \eqn{e_{ls}} (\eqn{kPa}).
#' @param ea Vapor pressure of the atmosphere, \eqn{e_{a}} (\eqn{kPa}).
#'
#' @export
#'
#' @examples
#'
norm_humidity <- function(els, ea){
  hn <- (els - ea) / els
  hn
}
