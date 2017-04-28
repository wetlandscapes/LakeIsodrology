#' Craig-Gordon model
#'
#' Implements the Craig-Gordon isotope evaporation model for an individual
#' element of water (H or O).
#'
#' The Craig-Gordon model as defined by Gat et al. (2001):
#'
#' \deqn{\delta_{E} = \frac{\alpha_{V/L}\delta_{L} - h_{N}\delta_{A} +
#' \epsilon_{V/L} + \epsilon_{diff}}{(1 - h_{N}) - \epsilon_{diff}}}
#'
#' Gat, J. R., Mook, W. G., & Meijer AJ. 2001. Environmental isotopes in the
#' hydrological cycle, principles and applications. Volume II: Atmospheric
#' water. IHP-V, Technical Document 2 (39): 1–113. Available at:
#' http://www.hydrology.nl/images/docs/ihp/Mook_II.pdf
#'
#' @param alpha_VL Equilibrium fractionation factor between vapor and liquid
#'   phases [-]. \eqn{\alpha_{V/L}}
#' @param Del_L Delta value for lake water (‰). \eqn{\delta_{L}}
#' @param hn Relative humidity normalized to saturation vapor pressure at the
#'   lake-atmosphere interface [-]. \eqn{h_{N}}
#' @param Del_A Delta value for the "free" atmosphere (‰). \eqn{\delta_{A}}
#' @param frac_VL Vapor-liquid equilibrium fractionation factor (‰).
#'   \eqn{\epsilon_{V/L}}
#' @param frac_diff Kinetic equilibrium fractionation factor (‰).
#'   \eqn{\epsilon_{diff}}
#'
#' @return Returns delta evaporation value for a particular water species (H or
#'   O) in permille.
#' @export
#'
#' @examples
#'
CraigGordon <- function(alpha_VL, Del_L, hn, Del_A, frac_VL, frac_diff){
  eq.top <- (alpha_VL * Del_L) - (hn * Del_A) - (frac_VL + frac_diff)
  eq.top <- (1 - hn) + frac_diff
  out <- eq.top / eq.bottom
  out
}
