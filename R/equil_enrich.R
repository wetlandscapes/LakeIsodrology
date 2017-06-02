#' Liquid-vapor equilibrium enrichment factor
#'
#' \code{equil_enrich} estimates the isotopic enrichment, \eqn{\epsilon_{V/L}}
#' (\eqn{\text{\textperthousand}}), given a known liquid-vapor equilibrium
#' fractionation.
#'
#' Enrichment is determined via (Horita et al. 2008):
#'
#' \deqn{\epsilon_{V/L} = (1 - \alpha_{V/L})10^{3}}
#'
#' Horita J, K Rozanski, S Cohen. 2008. Isotope effects in the evaporation of
#' water: A status report of the Craig-Gordon model. Isotopes in environmental
#' and health studies 44 (1): 23–49. DOI: 10.1080/10256010801887174.
#'
#' @param alpha_VL Liquid-vapor equilibrium fractionation factor,
#'   \eqn{\alpha_{V/L}} [\eqn{-}].
#'
#' @export
#'
#' @examples
#'
equil_enrich <- function(alpha_VL){
  e_VL <- (1 - alpha_VL) * 1e3
  e_VL
}
