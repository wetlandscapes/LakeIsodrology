#'Lake evaporation (Ryan-Harleman method)
#'
#'\code{E_RyanHarleman} returns the daily rate of evaporation, \eqn{E}
#'(\eqn{mm/day}), given the air temperature, lake surface temperature and
#'windspeed 2m above the lake surface.
#'
#'Daily evaporation is determined via (Ryan and Harleman 1973, Rasmussen et al.
#'1995, Rosenberry et al. 2007):
#'
#'\deqn{E = \frac{(a \lbrack |T_{s} - T_{a}| \rbrack ^{1/3} + b U)(e_{s} -
#'e_{a}) }{L \rho} 86.4}
#'
#'Rasmussen AH, Hondzo M, Stefan HG. 1995. A test of several evaporation
#'equations for water temperature simulations in lakes. JAWRA Journal of the
#'American Water Resources Association 31 (6): 1023–1028.
#'
#'Rosenberry DO, Winter TC, Buso DC, Likens GE. 2007. Comparison of 15
#'evaporation methods applied to a small mountain lake in the northeastern USA.
#'Journal of Hydrology 340 (3–4): 149–166. DOI: 10.1016/j.jhydrol.2007.03.018.
#'
#'Ryan PJ, Harleman DRF. 1973. An analytical and experimental study of transient
#'cooling pond behavior. Vol. 161. Dept. of Civil Engineering, Massachusetts
#'Institute of Technology.
#'
#'@param Ta Air temperature, \eqn{T_{a}} (\eqn{C}).
#'@param Ts Lake surface temperature, \eqn{T_{s}} (\eqn{C}).
#'@param U Wind speed at 2 m from the lake surface, \eqn{U} (\eqn{m/s}).
#'@param rho Density of water, \eqn{rho} (\eqn{kg m^{-3}}). Assumed to be 998
#'  \eqn{kg m^{-3}}.
#'@param a An empirical coefficient modifying the temperature gradient,
#'  [\eqn{-}]. Assumed to be 2.7.
#'@param b An emperical coefficent modifying the influence of wind, [\eqn{-}].
#'  Assumed to be 3.1.
#'
#'@export
#'
#' @examples
#'
E_RyanHarleman <- function(Ta, Ts, U, rho = 998, a = 2.7, b = 3.1){
  T.diff <- (abs(Ts - Ta)) ^ (1/3)
  T.part <- a * T.diff
  U.part <- b * U
  ea <- sat_vap(Ta)
  del <- slp_sat_vap(Ta, ea)
  es <- sat_vap_surf(ea, del, Ts, Ta)
  #Convert the saturated vapor pressure from kPa to mb, where 1 kPa = 10 mb.
  e.part <- (es * 10) - (ea * 10)
  conv <- 86.4 / (2.257 * rho)
  E <- (T.part + U.part) * e.part * conv
  E
}
