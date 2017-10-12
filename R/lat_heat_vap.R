#' Latent heat of vaporization
#'
#' \code{lat_heat_vap} returns the latent heat of vaporation
#'
#' @param Tl Lake temperature, \eqn{T_{l}} (\eqn{C}).
#'
#'
#' @export
#'
#'
lat_heat_vap <- function(Tl) {
  lambda <- 2.501 - (0.002361 * Tl)
  lambda
}
