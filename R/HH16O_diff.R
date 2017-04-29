#' Diffusion of HH16O
#'
#' \code{HH16O_diff} implements a temperature-dependent estimate of diffusion
#' for water - in particular HH16O.
#'
#' Diffusion of HH16O as determined by Holz et al. (2000):
#'
#' \deqn{ln(10^{9}D) = a_{1} + a_{2}(1000/T) + a_{3}(1000/T)^{2}}
#'
#' \tabular{rrrrrr}{
#' Where:\cr
#' \deqn{a_{1}} \tab \tab  1.67662250\cr
#' \deqn{a_{2}} \tab \tab  1.68167989\cr
#' \deqn{a_{3}} \tab \tab -0.577341011
#' }
#'
#' Holz M, SR Heil , A Sacco. 2000. Temperature-dependent self-diffusion
#' coefficients of water and six selected molecular liquids for calibration in
#' accurate 1H NMR PFG measurements. Physical Chemistry Chemical Physics 2 (20):
#' 4740 - 4742. DOI: 10.1039/b005319h.
#'
#' @param temperature Vector of air temperatures (C). \eqn{T}
#' @param Do Diffussion coefficient (1e-9 m^2 s^-1)
#' @param temp.s Standardized temperature (Kelvin)
#' @param gamma Fitted exponent [-]
#'
#' @return Returns a temperature-dependent estimate of molecular diffsuion for
#'   HH16O water (1e-9 m^2 s^-1).
#' @export
#'
#' @examples
#' Temps <- 288:308
#' HH16O.diffusion <- HH16O_diff(Temps)
#' ggplot2::qplot(Temps, HH16O.diffusion, geom = "point")

HH16O_diff <- function(temperature,
                       Do = 1.635e-08,
                       temp.s = 215.05,
                       gamma = 2.063){
  Temp.K <- temperature + 273.15
  Temp.ratio <- (Temp.K/temp.s) - 1
  Right.side <- Temp.ratio^gamma
  out <- Do * Right.side
  return(out)
}
