#' Lake evaporation (Thornethwaite method)
#'
#' \code{E_Thornthwaite} returns the daily evaporation, \eqn{E} (\eqn{mm
#' day^{-1}}) using temperature data.
#'
#' \eqn{E} is determined via (Thornthwaite 1948, Rosenberry et al. 2007):
#'
#' \deqn{E = \bigg(a \bigg(\frac{10 T_{a}}{I} \bigg)^{b I^{3} - c I^{2} + d I +
#' e} \bigg) \frac{10}{day}}
#'
#' Thornthwaite CW. 1948. An approach toward a rational classification of
#' climate. Geographical Review 38 (1): 55. DOI: 10.2307/210739.
#'
#' Rosenberry DO, Winter TC, Buso DC, Likens GE. 2007. Comparison of 15
#' evaporation methods applied to a small mountain lake in the northeastern USA.
#' Journal of Hydrology 340 (3–4): 149–166. DOI: 10.1016/j.jhydrol.2007.03.018.
#'
#' @param Ta Air temperature, \eqn{T_{a}} (\eqn{C}).
#' @param I Annual heat index, \eqn{I} (\eqn{-}).
#' @param day Number of days in a month, \eqn{day} (\eqn{days}).
#' @param a An emperical coefficient, assumed to be 1.6.
#' @param b An emperical coefficient, assumed to be 6.75e-7.
#' @param c An emperical coefficient, assumed to be 7.71e-5.
#' @param d An emperical coefficient, assumed to be 1.79e-2.
#' @param e An emperical coefficient, assumed to be 0.49.
#' @param conv A multiplier that converts base units to mm/day. It is assumed to
#'  be 10, but will need to be adjust for alternative units.
#'
#' @export
#'
#' @examples
#'
E_Thornthwaite <- function(Ta, I, day, a = 1.6, b = 6.75e-7, c = 7.71e-5, d = 1.79e-2, e = 0.49, conv = 10){
  exp.part <- (b * (I ^ 3)) - (c * (I ^ 2)) + (d * I) + e
  Ta.part <- (conv * Ta / I) ^ exp.part
  E <- a * Ta.part * (conv / day)
  E <- ifelse(is.nan(E), 0, E)
  E
}
