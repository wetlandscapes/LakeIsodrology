#' Lake evaporation (Mass Transfer method)
#'
#' \code{E_MassTransfer} estimates a daily rate of evaporation, \eqn{E}
#' (\eqn{mm/day}), using a Dalton-type approach.
#'
#' Evaporation as determined by (Harbeck et al. 1958, Rosenberry et al. 2007):
#'
#' \deqn{E = (N U (e_{ls} - e_{a})) \cdot 10}
#'
#' Harbeck GE, Kohler MA, Koberg GE. 1958. Water-loss investigations: Lake Mead
#' studies Available at: http://pubs.er.usgs.gov/publication/pp298.
#'
#' Rosenberry DO, Winter TC, Buso DC, Likens GE. 2007. Comparison of 15
#' evaporation methods applied to a small mountain lake in the northeastern USA.
#' Journal of Hydrology 340 (3–4): 149–166. DOI: 10.1016/j.jhydrol.2007.03.018.
#'
#' @param U Windspeed 2 m above the lake surface, \eqn{U} (\eqn{m s^{-1}}).
#' @param els Saturated vapor pressure at the lake surface, \eqn{e_{ls}}
#'   (\eqn{kPa}).
#' @param ea Atmospheric vapor pressure, \eqn{e_{a}} (\eqn{kPa}).
#' @param N Empirical parameter, no standard value.
#'
#' @export
#'
#' @examples
#'
E_MassTransfer <- function(U, els, ea, N){
  #Convert the saturated vapor pressure from kPa to mb, where 1 kPa = 10 mb.
  inner <- (els * 10) - (ea * 10)
  outer <- N * U * inner
  out <- outer * 10
  out
}
