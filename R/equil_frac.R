#' Liquid-vapor equilibrium fractionation
#'
#' Estimates the temperature-dependent equilibrium fracitonation factor (aka,
#' isotope effect), \eqn{\alpha_{V/L}}, of common water isotopes (i.e.,
#' 2-hydrogen and 18-oxygen) between the liquid and vapor phases. Units of the
#' output \eqn{\alpha} value are in decimal notation [-].
#'
#' Fractionation s determined via (Majoube 1971):
#'
#' Hydrogen: \eqn{10^{3} ln\alpha_{V/L} = 24.844(10^{6}/(T + 273.15)^{2}) -
#' 76.248(10^{3}/(T + 273.15)^{2}) + 52.612}
#'
#' Oxygen: \eqn{10^{3} ln\alpha_{V/L} = 1.137(10^{6}/(T + 273.15)^{2}) -
#' 0.4156(10^{3}/(T + 273.15)) - 2.0667}
#'
#' Majoube M. 1971. Oxygen-18 and deuterium fractionation between water and
#' steam (in French). Journal de Chimie Physique et de Physico-Chimie Biologique
#' 68: 1423–1436.
#'
#' @param temperature Numeric air temperature value, \eqn{T} (\eqn{C}).
#' @param element Character indicating "Hydrogen" or "Oxygen".
#'
#' @export
#'
#' @examples
#'
equil_frac <- function(temperature, element){
  temp_K <- temperature + 273.15
  if(element == "Hydrogen"){
    first <- 24.844 * ((1e+06) / ((temp_K^2)))
    second <- -76.248 * (1e+03)/(temp_K)
    frac_factor <-  first +  second + 52.612
  } else if (element == "Oxygen"){
    first <- 1.137 * ((1e+06) / ((temp_K^2)))
    second <- -0.4156 * (1e+03)/(temp_K)
    frac_factor <-  first +  second + -2.0667
  } else {
    stop(paste("Something went wrong with the isotope selection."))
  }
  frac_factor <- frac_factor/1000
  frac_factor <- exp(frac_factor)
  frac_factor
}
