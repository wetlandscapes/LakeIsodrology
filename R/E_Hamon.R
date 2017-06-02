#' Lake evaporation (Hamon method)
#'
#' \code{E_Hamon} provides an estimate of daily evaporation, \eqn{E} (\eqn{mm
#' day^{-1}}) using data related to day length.
#'
#' Daily evaporation is determined via (Hamon 1961, Rosenberry et al. 2007):
#'
#' \deqn{a \bigg(\frac{D}{12}\bigg)^{2} \frac{\rho_{s}}{100} \cdot 25.4}
#'
#' Hamon, WR. 1961. Estimating potential evapotranspiration. Proc. Amer. Soc.
#' civ. Engrs. 87, 107–120.
#'
#' Rosenberry DO, Winter TC, Buso DC, Likens GE. 2007. Comparison of 15
#' evaporation methods applied to a small mountain lake in the northeastern USA.
#' Journal of Hydrology 340 (3–4): 149–166. DOI: 10.1016/j.jhydrol.2007.03.018.
#'
#' @param D Hours of daylight, \eqn{D} (\eqn{hrs}).
#' @param ps saturation vapor density, \eqn{\rho_{s}} (\eqn{g m^{-3}}).
#' @param a An empirical coefficient, assumed to be 0.55.
#'
#' @export
#'
#' @examples
#'
E_Hamon <- function(D, ps, a = 0.55){
  pt1 <- (D / 12) ^ 2
  E <- a * pt1 * ps / 100 * 25.4
  E
}
