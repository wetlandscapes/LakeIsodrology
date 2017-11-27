#' Liquid-vapor equilibrium enrichment factor
#'
#' \code{equil_enrich} estimates the isotopic enrichment or depletion,
#' \eqn{\epsilon^*_{LV}} [\eqn{-}], given a known liquid-vapor equilibrium
#' fractionation factor.
#'
#' Enrichment is determined via (Horita et al. 2008):
#'
#' \deqn{\epsilon^*_{LV} = (\alpha^*_{LV} - 1)}
#'
#' Note that if enirchment/depletion associated with vapor-liquid equilibrium is
#' desired, \eqn{\epsilon^*_{VL}}, then one can either input
#' \eqn{\alpha^*_{VL}} or \eqn{\frac{1}{\alpha^*_{LV}}} into this function.
#'
#' Horita J, K Rozanski, S Cohen. 2008. Isotope effects in the evaporation of
#' water: A status report of the Craig-Gordon model. Isotopes in environmental
#' and health studies 44 (1): 23–49. DOI: 10.1080/10256010801887174.
#'
#' @param alpha_VL Liquid-vapor equilibrium fractionation factor,
#'   \eqn{\alpha_{V/L}} [\eqn{-}].
#'
#' @return A numeric value
#'
#' @export
#'
#' @examples
#'
equil_enrich <- function(alpha_VL){
  e_VL <- (alpha_VL - 1)
  e_VL
}
