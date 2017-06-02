#'Lake evaporation (Ryan-Harleman method)
#'
#'\code{E_RyanHarleman} returns the daily rate of evaporation, \eqn{E}
#'(\eqn{mm/day}), given the air temperature, lake surface temperature and
#'windspeed 2m above the lake surface.
#'
#'Daily evaporation is determined via (Ryan and Harleman 1973, Rasmussen et al.
#'1995, Rosenberry et al. 2007):
#'
#'\deqn{E = \frac{(a \lbrack T_{ls} - T_{a} \rbrack ^{b} + c U)(e_{ls} - e_{a})
#'}{\lambda_{v} \rho} 86.4}
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
#'@param Tls Lake surface temperature, \eqn{T_{ls}} (\eqn{C})
#'@param U Wind speed at 2 m from the lake surface, \eqn{U} (\eqn{m/s}).
#'@param els Saturated vapor pressure at the lake surface, \eqn{e_{ls}}
#'  (\eqn{kPa}).
#'@param ea Atmospheric vapor pressure, \eqn{e_{a}} (\eqn{kPa}).
#'@param L Latent heat of vaporization, \eqn{\lambda_{v}} (\eqn{MJ kg^{-1}}).
#'  Assumed to be 2.47.
#'@param rho Density of water, \eqn{rho} (\eqn{kg m^{-3}}). Assumed to be 998
#'  \eqn{kg m^{-3}}.
#'@param a An empirical coefficient modifying the temperature gradient. Assumed
#'  to be 2.7.
#'@param b Another empirical coefficient modifying the temperature gradient.
#'  Assumed to be 1/3.
#'@param c An emperical coefficent modifying the influence of wind. Assumed to
#'  be 3.1.
#'
#'@export
#'
#' @examples
#' #Calculate the saturation vapor pressure for a given temperature.
#' es <- sat_vap(Ta)
#' #Determine the slope of the saturation vapor-pressure curve.
#' del <- slp_sat_vap(Ta, es)
#' #Use these data, in addition to the surface temperature of the lake, to determine
#' # the vapor pressure at the lake's surface.
#' els <- sat_vap_surf(es, del, Tls, Ta)
#' #Apply values to the Ryan-Harleman evaporation function:
#' E_RyanHarleman(Ta, Tls, U, els, ea)
#'
E_RyanHarleman <- function(Ta, Tls, U, els, ea, L = 2.47, rho = 998, a = 2.7, b = 1/3, c = 3.1){
  T.diff <- (abs(Tls - Ta)) ^ b
  T.part <- a * T.diff
  U.part <- c * U
  #Convert the saturated vapor pressure from kPa to mb, where 1 kPa = 10 mb.
  e.part <- (els * 10) - (ea * 10)
  conv <- 86.4 / (2.257 * rho)
  E <- (T.part + U.part) * e.part * conv
  E
}
