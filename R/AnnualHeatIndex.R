#' Annual Heat Index
#'
#' \code{AnnualHeatIndex} returns the annual heat index, \eqn{I} used by the
#' Thornthwaite evaporation method.
#'
#' \eqn{I} is determined by (Thornthwaite 1948, Rosenberry et al. 2007):
#'
#' \deqn{I = \sum_{i = 1}^{365} \bigg(\frac{T_{a,i}}{5}\bigg)^{a}}
#'
#' Thornthwaite CW. 1948. An approach toward a rational classification of
#' climate. Geographical Review 38 (1): 55. DOI: 10.2307/210739.
#'
#' Rosenberry DO, Winter TC, Buso DC, Likens GE. 2007. Comparison of 15
#' evaporation methods applied to a small mountain lake in the northeastern USA.
#' Journal of Hydrology 340 (3–4): 149–166. DOI: 10.1016/j.jhydrol.2007.03.018.
#'
#' @param Ta Air temperature, \eqn{T_{a}} (\eqn{C}).
#' @param a An emperical coefficient, assumed to be 1.514.
#'
#' @export
#'
#' @examples
#'
AnnualHeatIndex <- function(Ta, a = 1.514){
  i <- (Ta/5) ^ a
  #Do not consider negative values.
  i <- ifelse(is.nan(i), 0, i)
  I <- sum(i)
  I
}
