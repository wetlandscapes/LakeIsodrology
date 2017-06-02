#' Lake evaporation (Blaney-Criddle method)
#'
#' \code{E_BlaneyCriddle} returns the daily rate of evaporation, \eqn{E}
#' (\eqn{mm/day}), using data related to temperature and day length.
#'
#' Evaporation is determined by (Blaney and Criddle 1962, McGuinness and Bordne
#' 1972, Rosenberry et al. 2007):
#'
#' \deqn{E = (a T_{a} - b) \cdot T_{a} \cdot \frac{D}{D_{a}} \cdot c}
#'
#' Blaney H, Criddle W. 1962. Determining consumptive use and irrigation water
#' requirements. US Department of Agriculture Technical Bulletin 1275.
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
#' @param D Hours of daylight, \eqn{D} (\eqn{hrs}).
#' @param Da Maximum annual hours of daylight, \eqn{D_{a}} (\eqn{hrs}).
#' @param a An emperical coefficient modifying the influence of temperature.
#'   Assumed 0.0173.
#' @param b Also an emperical coefficient modifying the influence of air
#'   temperature. Assumed 0.314
#'
#' @export
#'
#' @examples
#'
E_BlaneyCriddle <- function(Ta, D, Da, a = 0.0173, b = 0.314){
  pt1 <- (a * Ta)
  pt2 <- pt1 - b
  E <- pt2 * Ta * (D / DTa) * 25.4
  E
}
