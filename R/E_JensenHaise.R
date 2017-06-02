#' Lake evaporation (Jensen-Haise method)
#'
#' \code{E_JensenHaise} returns the daily rate of evaporation, \eqn{E}
#' (\eqn{mm/day}), given the air temperature and solar radiation.
#'
#' Evaporation as determined by (Jensen and Haise 1963, McGuinness and Bordne
#' 1972, Rosenberry et al. 2007):
#'
#' \deqn{E = (a T_{a} - b)(Q_{s} c)}
#'
#' Note that this method is very closely related to that of Stephens and Stewart
#' (1963), the major difference being the value of the assumed parameters. For
#' Stephens and Stewart the values of a, b, and c are 0.0082, 0.19, and
#' 3.495e-2, respectively.
#'
#' Jensen, ME, Haise HR. 1963. Estimating evapotranspiration from solar
#' radiation. Proceedings of the American Society of Civil Engineers, Journal of
#' the Irrigation and Drainage Division 89: 15-41.
#'
#' Stephens JC, Stewart EH. 1963. A comparison of procedures for computing
#' evaporation and evapotranspiration. In Publication 62, International
#' Association of Scientific Hydrology. International Union of Geodynamics and
#' GeophysicsBerkeley, CA; 123–133.
#'
#' McGuinness JL, Bordne EF. 1972. A comparison of lysimeter-derived potential
#' evapotranspiration with computed values. Technical Bulletin 1452, US
#' Department of Agriculture Agricultural Research Service, Washington, DC.
#'
#' Rosenberry DO, Winter TC, Buso DC, Likens GE. 2007. Comparison of 15
#' evaporation methods applied to a small mountain lake in the northeastern USA.
#' Journal of Hydrology 340 (3–4): 149–166. DOI: 10.1016/j.jhydrol.2007.03.018.
#'
#' @param Ta Air temperature, \eqn{T_{a}} (\eqn{C}).
#' @param Qs Solar radiation, \eqn{Q_{s}} (\eqn{W m^{-2}}).
#' @param a Empirical parameter, assumed to be 0.014.
#' @param b Empirical parameter, assumed to be 0.37.
#' @param c Empirical parameter, assumed to be 0.03523.
#'
#' @export
#'
#' @examples
#'
E_JensenHaise <- function(Ta, Qs, a = 0.014, b = 0.37, c = 3.523e-2){
  Ta.F <- (Ta * 9 / 5) + 32
  left <- ((a * Ta.F) - b)
  right <- Qs * c
  E <- left * right
  E <- ifelse(E < 0, 0, E)
  E
}
