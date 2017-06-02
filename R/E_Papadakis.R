#' Lake evaporation (Papadakis method)
#'
#' \code{E_Papadakis} returns an estimate of evaporation, \eqn{E} (\eqn{mm
#' day^{-1}}), based on data related to saturation vapor pressure and number of
#' days in a month.
#'
#' \eqn{E} is determined by (Papadakis 1965, McGuinness and Bordne 1972,
#' Rosenberry et al. 2007):
#'
#' \deqn{E = a (e_{s,max} - [e_{s,min} - b]) \bigg(\frac{10}{day} \bigg)}
#'
#' Papadakis J. 1965. Potential evapotranspiration. Bueuos Aires, 54 pp.
#'
#' McGuinness JL, Bordne EF. 1972. A comparison of lysimeter-derived potential
#' evapotranspiration with computed values. Technical Bulletin 1452, US
#' Department of Agriculture Agricultural Research Service, Washington, DC.
#'
#' Rosenberry DO, Winter TC, Buso DC, Likens GE. 2007. Comparison of 15
#' evaporation methods applied to a small mountain lake in the northeastern USA.
#' Journal of Hydrology 340 (3–4): 149–166. DOI: 10.1016/j.jhydrol.2007.03.018.
#'
#' @param esmax Maximum saturation vapor pressure for a given day,
#'   \eqn{e_{s,max}} (\eqn{kPa}).
#' @param esmin Minimum saturation vapor pressure for a given day,
#'   \eqn{e_{s,min}} (\eqn{kPa}).
#' @param day Number of days in a month, \eqn{day} (\eqn{days}).
#' @param a An emperical coefficient, assumed to be 0.5625.
#' @param b An emperical coefficient, assumed to be 2.
#'
#' @export
#'
#' @examples
#'
E_Papadakis <- function(esmax, esmin, day, a = 0.5625, b = 2){
  #Convert kPa to Pa.
  esmax.Pa <- esmax * 1000
  esmin.Pa <- esmin * 1000
  es.part <- (esmax.Pa - (esmin.Pa - b))
  E <- a * es.part * (10 / day)
  E
}
