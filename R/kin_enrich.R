#' Liquid-vapor kinetic enrichment factor
#'
#' \code{kin_enrich} calculates the kinetic enrichment, \eqn{\epsilon_{diff}}
#' (\eqn{\text{\textperthousand}}), that occurs, given the normalized relative humidity and
#' self-diffusion of a system.
#'
#' Kinetic enrichment is formulated as (Gat et al. 2001, Gibson et al. 2008,
#' Horita et al. 2008):
#'
#' \deqn{\epsilon_{diff} = n (1 - h_{n}) \theta \cdot C_{K}}
#'
#' Note that this function assumes the study lakes have little influence on the
#' atmospheric boundary layer (\eqn{\theta = 1}), which may not be true for
#' larger lakes (Horita et al. 2008). The function also assumes a roughness
#' coefficent, \eqn{n}, is 0.5 representing a rough surface.
#'
#' Gat, JR, WG Mook, AJ Meijer. 2001. Environmental isotopes in the hydrological
#' cycle, principles and applications. Volume II: Atmospheric water. IHP-V,
#' Technical Document 2 (39): 1–113.
#'
#' Gibson JJ, SJ Birks, TWD Edwards. 2008. Global prediction of δA and δ2H-δ18O
#' evaporation slopes for lakes and soil water accounting for seasonality.
#' Global Biogeochemical Cycles 22 (2): 1–12. DOI: 10.1029/2007GB002997.
#'
#' Horita J, K Rozanski, S Cohen. 2008. Isotope effects in the evaporation of
#' water: A status report of the Craig-Gordon model. Isotopes in environmental
#' and health studies 44 (1): 23–49. DOI: 10.1080/10256010801887174.
#'
#'
#' @param hn Normalize relative humidity, \eqn{h_{n}} [\eqn{-}].
#' @param Ck Self-diffussion ratio, \eqn{C_{K}} (\eqn{\text{\textperthousand}}).
#' @param n Roughness coefficient, \eqn{n} [\eqn{-}]. Assumed 0.5.
#' @param theta Atmospheric influence of the study lake, \eqn{\theta} [\eqn{-}].
#'   Assumed 1, but will be smaller for verylarge lakes.
#'
#' @export
#'
#' @examples
#'
kin_enrich <- function(hn, Ck, n = 0.5, theta = 1){
  frac_diff <- n * (1 - hn) * theta * Ck
  frac_diff
}
