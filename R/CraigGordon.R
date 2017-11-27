#' Craig-Gordon model to estimate evaporation isotope values
#'
#' Implements the Craig-Gordon isotope evaporation model for an individual
#' isotope of water, hydrogen or oxygen. Units of the output delta value, \eqn{\delta_{E}},
#' are provided in decimal form.
#'
#' The Craig-Gordon model (similar to that defined by Gat et al. (2001)):
#'
#' \deqn{\delta_{E} = \frac{(\delta_{L}/\alpha^*_{LV}) - h_{N}\delta_{A} +
#' \epsilon^*_{VL} + \epsilon^{\kappa}}{(1 - h_{N}) - \epsilon^{\kappa}}}
#'
#' Gat, JR, WG Mook, AJ Meijer. 2001. Environmental isotopes in the hydrological
#' cycle, principles and applications. Volume II: Atmospheric water. IHP-V,
#' Technical Document 2 (39): 1–113.
#'
#' @param dL Delta value of a liquid (e.g., lake), \eqn{\delta_{L}}
#'   [\eqn{-}].
#' @param dA Delta value for the "free" atmosphere \eqn{\delta_{A}},
#'   [\eqn{-}].
#' @param aeLV Equilibrium fractionation factor between liquid and vapor
#'   phases, \eqn{\alpha^*_{LV}} [\eqn{-}].
#' @param eEVL Vapor-liquid equilibrium fractionation factor,
#'   \eqn{\epsilon^*_{VL}} [\eqn{-}].
#' @param frac_diff Kinetic equilibrium fractionation factor,
#'   \eqn{\epsilon^\kappa}, [\eqn{-}].
#' @param hn Relative humidity normalized to saturation vapor pressure at the
#'   lake-atmosphere interface, \eqn{h_{N}} [\eqn{-}].
#'
#' @return Numeric value
#' @export
#'
#' @examples
#'
CraigGordon <- function(dL, dA, aELV, eEVL, eK, hn){
  if(!is.numeric(dL, dA, aELV, eEVL, eK, hn)) {
    stop('Expecting all input variables to be numeric.')
  }
  if(aELV < 1) {
    warning('"aELV" may be too low.')
  }
  if(aELV > 1) {
    warning('"aELV" may be too high.')
  }
  if(eEVL > 0) {
    warning('"eEVL" seems high.')
  }
  if(eK > 0) {
    warning('"eK" seems high.')
  }
  if(hN > 1 | hN < 0) {
    warning('hN is outside the expected bounds or 0 and 1.')
  }
  top <- (dL / aELV) - (hn * dA) + eEVL + eK
  bot <- 1 - hn - eK
  out <- top / bot
  out
}
