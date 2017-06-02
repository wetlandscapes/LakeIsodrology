#'Lake evaporation (Makkink method)
#'
#'\code{E_Makkink} returns the evaporation rate, \eqn{E} (\eqn{mm/day}), given
#'the slope of the saturation vapor pressure curve, psychrometric constant, and
#'solar radiation.
#'
#'Daily evaporation is estimate by (Makkink 1957, McGuinness and Bordne 1972,
#'Rosenberry et al. 2007):
#'
#'\deqn{E = \bigg( \bigg(a \frac{\Delta}{\Delta + \gamma}
#'\frac{Q_{s}}{\lambda_{v} \rho}\bigg) - b \bigg)}
#'
#'Makkink, GF. 1957. Testing the Penman formula by means of lysimeters. J. Inst.
#'Water Eng 11(3): 277-288.
#'
#'McGuinness JL, Bordne EF. 1972. A comparison of lysimeter-derived potential
#'evapotranspiration with computed values. Technical Bulletin 1452, US
#'Department of Agriculture Agricultural Research Service, Washington, DC.
#'
#'Rosenberry DO, Winter TC, Buso DC, Likens GE. 2007. Comparison of 15
#'evaporation methods applied to a small mountain lake in the northeastern USA.
#'Journal of Hydrology 340 (3–4): 149–166. DOI: 10.1016/j.jhydrol.2007.03.018.
#'
#'@param del Slope of the saturation vapor pressure curve, \eqn{\Delta}
#'  (\eqn{kPa C^{-1}}).
#'@param gamma Psychrometric constant, \eqn{\gamma} (\eqn{kPa C^{-1}}).
#'@param Qs Solar radiation, \eqn{Q_{s}} (\eqn{W m^{-2}}).
#'@param L Latent heat of vaporization, \eqn{\lambda_{v}} (\eqn{MJ kg^{-1}}).
#'  Assumed to be 2.47.
#'@param rho Density of water at a particular temperature, \eqn{\rho} (\eqn{kg
#'  m^{-3}}). Assumed to be 988 (density at 20 C).
#'@param a Empirical parameter, assumed to be 52.6.
#'@param b Empirical parameter, assumed to be 0.12.
#'
#'
#'@export
#'
#' @examples
#'
E_Makkink <- function(del, gamma, Qs, L = 2.47, rho = 998, a = 52.6, b = 0.12){
  pt1 <- del / (del + gamma)
  pt2 <- Qs / (L * rho)
  left <- a * pt1 * pt2
  out <- left - b
  out
}
