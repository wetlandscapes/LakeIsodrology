#'Liquid-vapor equilibrium fractionation
#'
#'Estimates the temperature-dependent equilibrium fracitonation of common water
#'isotopes.
#'
#'Fractionation s determined via (Majoube 1971):
#'
#'Hydrogen: \eqn{10^{3}ln\alpha = 24.844(10^{6}/T_{K}^{2}) -
#'76.248(10^{3}/T_{K}^{2}) + 52.612}
#'
#'Oxygen: \eqn{10^{3}ln\alpha = 1.137(10^{6}/T_{K}^{2}) -
#'0.4156(10^{3}/T_{K}^{2}) - 2.0667}
#'
#'Majoube M. 1971. Oxygen-18 and deuterium fractionation between water and steam
#'(in French). Journal de Chimie Physique et de Physico-Chimie Biologique 68:
#'1423–1436
#'
#'@param temperature Numeric value of absolute temperature (Kelvin)
#'@param element Character indicating "Hydrogen" or "Oxygen"
#'
#'@return Returns the fractionation factor (aka, isotope effect) between liquid
#'  and vapor, under equilibrium conditions [-].
#'@export
#'
#' @examples
#'
equil_frac <- function(temperature, element){
  if(element == "Hydrogen"){
    first <- 24.844 * ((1e+06) / ((temperature^2)))
    second <- -76.248 * (1e+03)/(temperature)
    frac_factor <-  first +  second + 52.612
  } else if (element == "Oxygen"){
    first <- 1.137 * ((1e+06) / ((temperature^2)))
    second <- -0.4156 * (1e+03)/(temperature)
    frac_factor <-  first +  second + -2.0667
  } else {
    stop(paste("Something went wrong with the isotope selection."))
  }
  frac_factor <- frac_factor/1000
  frac_factor <- exp(frac_factor)
  frac_factor
}
