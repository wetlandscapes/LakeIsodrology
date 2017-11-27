#' Convert fractional abundance to isotope ratio
#'
#' \code{fraction_to_ratio} converts the fractional abundance of a heavy isotope
#' to its corresponding isotope ratio. For example, the fracitonal abundance of
#' \eqn{{}^{18}O} is \eqn{{}^{18}F = {}^{18}O / ({}^{16}O + {}^{18}O)}.
#'
#' \deqn{{}^{A}R = {}^{A}F (1 - {}^{A}F)}
#'
#' Where A is the heavy isotope of interest, F is the fractional abundance
#' \eqn{[-]}, and R is the isotope ratio \eqn{[-]}.
#'
#' @param fracAbund The fractional abundance of a heavy isotope, \eqn{[-]}.
#'
#' @return A unitless numeric value.
#' @export
#'
#' @examples
fraction_to_ratio <- function(fracAbund) {
  if(!is.numeric(fracAbund)) {
    stop("This function requires numeric values to work.")
  }
  if(fracAbund > 1 | fracAbund <= 0) {
    warning("The fractional abundance value(s) is either higher or lower than expected.")
  }
  out <- fracAbund / (1 - fracAbund)
  return(out)
}


#' Convert isotope ratio to fractional abundance
#'
#' \code{ratio_to_fraction} is the inverse of \code{fraction_to_ratio}. For more
#' information about this function, see documentation for the latter.
#'
#' Equation used to convert from an isotope ratio to fractional abundance:
#'
#' \deqn{{}^{A}F = {}^{A}R(1 + {}^{A}R)}
#'
#' Where \eqn{A} is the isotope of interest (usually the heavy, or less common
#' one), \eqn{F} is the fractional abundance between the rare and common
#' isotope, and \eqn{R} is the isotope ratio.
#'
#' NOTE: This function assumes only two isotopes are important in the system.
#'
#' @param ratio The isotope ratio of a heavy istope, \eqn{[-]}.
#'
#' @return A numeric value representing an isotope ratio.
#' @export
#'
#' @examples
ratio_to_fraction <- function(ratio) {
  if(!is.numeric(ratio)) {
    stop("This function requires numeric values to work.")
  }
  if(ratio > 1 | ratio <= 0) {
    warning("The ratio value(s) is either higher or lower than expected.")
  }
  out <- ratio * (1 + ratio)
  return(out)
}


#' Convert isotope ratio to delta value
#'
#' This function, \code{ratio_to_delta} takes the heavy isotope ratio of a known
#' element, either hydrogen or oxygen, and converts it a delta value (per mille)
#' for a given standard. Applicable standards include Vienna Standard Mean Ocean
#' Water (VSMOW), Greenland Ice Sheet Precipitation (GISP), and Standard Light
#' Antarctic Precipitation (SLAP).
#'
#' The relationship used to convert a given isotope ratio to delta notation:
#'
#' \deqn{\delta^{A}X = \Bigg(\frac{{}^{A}R}{{}^{A}R_{Standard}} - 1\Bigg) 1000}
#'
#' Where \eqn{A} indicates the isotope of element \eqn{X}, \eqn{R} is the ratio
#' of the sample, and \eqn{R_{Standard}} is the isotope ratio of the standard
#' being used.
#'
#' Below are the values of each standard and its isotope ratio used in this
#' function.
#'
#' \tabular{lrr}{
#' Standard \tab 2H/1H \tab 18O/16O\cr
#' VSMOW \tab 0.00015576 \tab 0.00200520\cr
#' GISP \tab 0.00012624 \tab 0.00195555\cr
#' SLAP \tab 0.00008902 \tab 0.00189391 }
#'
#' VSMOw and SLAP standard information can be found at:
#' https://nucleus.iaea.org/rpst/documents/VSMOW_SLAP.pdf
#'
#' GISP standard information:
#' https://nucleus.iaea.org/rpst/Documents/rs_GISP.pdf
#'
#'
#' @param ratio Heavy isotope ratio of either hydrogen or oxygen, \eqn{[-]}.
#' @param element Character value indicating either "Hydrogen" or "Oxygen".
#' @param standard Character value indicating the acronym of the standard to be
#'   used: "VMSOW", "GISP", or "SLAP".
#' @param unit Character value explicitly stating if the output value should be
#'   in "decimal" or "permille" units.
#'
#' @return A numeric delta value in either permille or decimal units.
#' @export
#'
#' @examples
ratio_to_delta <- function(ratio, element, standard = "VSMOW", unit) {
  if(!is.numeric(ratio)){
    stop("Isotope ratio must be a numeric value.")
  }
  if(ratio < 0) {
    warning("The isotope ratio is negative.")
  }
  if(element != "Hydrogen" & element != "Oxygen") {
    stop("Element must a character value indicating either 'Hydrogen' or 'Oxygen'.")
  }
  if(standard != "VSMOW" & standard != "GISP" & standard != "SLAP" ) {
    stop("Can only evaluate one of the three common water standards: VSMOW, GISP, or SLAP.")
  }
  if(unit != "permille" & unit != "decimal") {
    stop("Expecting unit values to be either 'permille' or 'decimal'.")
  }
  if(element == "Hydrogen") {
    if(standard == "VSMOW") {
      #Hydrogen - VSMOW
      Rs <- 155.76 * 1e-6
    } else if(standard == "GISP") {
      #Hydrogen - GISP
      Rs <- 126.24 * 1e-6
    } else {
      #Hydrogen - SLAP
      Rs <- 89.02 * 1e-6
    }
  } else {
    if(standard == "VSMOW") {
      #Oxygen - VSMOW
      Rs <- 2005.20 * 1e-6
    } else if(standard == "GISP") {
      #Oxygen - GISP
      Rs <- 1955.55 * 1e-6
    } else {
      #Oxygen - SLAP
      Rs <- 1893.91 * 1e-6
    }
  }
  delta <- ((ratio / Rs) - 1)
  if(unit == "permille") {
    delta <- delta * 1000
  }
  return(delta)
}


#' Convert delta value to isotope ratio
#'
#' This function, \code{delta_to_ratio} takes a delta value, in per mille of
#' either hydrogen or oxygen, and converts it to an isotope ratio [-], using a
#' standard. Applicable standards include Vienna Standard Mean Ocean Water
#' (VSMOW), Greenland Ice Sheet Precipitation (GISP), and Standard Light
#' Antarctic Precipitation (SLAP).
#'
#' The relationship used to convert a given delta value to an isotope ratio:
#'
#' \deqn{{}^{A}R = \Bigg(\frac{\delta^{A}X}{1000} + 1\Bigg) {}^{A}R_{Standard}}
#'
#' Where \eqn{A} indicates the isotope of element \eqn{X}, \eqn{R} is the ratio
#' of the sample, and \eqn{R_{Standard}} is the isotope ratio of the standard
#' being used.
#'
#' Below are the values of each standard and its isotope ratio used in this
#' function.
#'
#' \tabular{lrr}{
#' Standard \tab 2H/1H \tab 18O/16O\cr
#' VSMOW \tab 0.00015576 \tab 0.00200520\cr
#' GISP \tab 0.00012624 \tab 0.00195555\cr
#' SLAP \tab 0.00008902 \tab 0.00189391 }
#'
#' VSMOw and SLAP standard information can be found at:
#' https://nucleus.iaea.org/rpst/documents/VSMOW_SLAP.pdf
#'
#' GISP standard information:
#' https://nucleus.iaea.org/rpst/Documents/rs_GISP.pdf
#'
#' @param delta An isotope delta value, in per mille.
#' @param element Character value indicating either "Hydrogen" or "Oxygen".
#' @param standard Character value indicating the acronym of the standard to be
#'   used: "VMSOW", "GISP", or "SLAP".
#' @param unit Character value explicitly indicating if the input units are
#'   "decimal" or "permille".
#'
#' @return Unitless numeric value of the isotope ratio.
#' @export
#'
#' @examples
delta_to_ratio <- function(delta, element, standard = "VSMOW", unit) {
  if(!is.numeric(delta)){
    stop("Isotope ratio must be a numeric value.")
  }
  if(element != "Hydrogen" & element != "Oxygen") {
    stop("Element must a character value indicating either 'Hydrogen' or 'Oxygen'.")
  }
  if(standard != "VSMOW" & standard != "GISP" & standard != "SLAP" ) {
    stop("Can only evaluate one of the three common water standards: VSMOW, GISP, or SLAP.")
  }
  if(unit != "permille" & unit != "decimal") {
    stop("Expecting unit values to be either 'permille' or 'decimal'.")
  }
  if(element == "Hydrogen") {
    if(standard == "VSMOW") {
      #Hydrogen - VSMOW
      Rs <- 155.76 * 1e-6
    } else if(standard == "GISP") {
      #Hydrogen - GISP
      Rs <- 126.24 * 1e-6
    } else {
      #Hydrogen - SLAP
      Rs <- 89.02 * 1e-6
    }
  } else {
    if(standard == "VSMOW") {
      #Oxygen - VSMOW
      Rs <- 2005.20 * 1e-6
    } else if(standard == "GISP") {
      #Oxygen - GISP
      Rs <- 1955.55 * 1e-6
    } else {
      #Oxygen - SLAP
      Rs <- 1893.91 * 1e-6
    }
  }
  if(unit == "permille"){
    ratio <- ((delta / 1000) + 1) * Rs
  } else {
    ratio <- (delta + 1) * Rs
  }
  return(ratio)
}


#' Permille to decimal conversion
#'
#' As a convenience, \code{permille_to_decimal} converts delta values from units
#' of permille to decimal values.
#'
#' @param delta A \eqn{\delta} value in units of permille.
#'
#' @return Numeric value.
#' @export
#'
#' @examples
permille_to_decimal <- function(delta) {
  if(!is.numeric(delta)) {
    stop("Expecting a numeric delta value.")
  }
  out <- delta * 1000
  return(out)
}


#' Decimal to permille converstion
#'
#' A convenience function that converts a decimal delta value to units of
#' permillle.
#'
#' @param delta A \eqn{\delta} value in decimal units.
#'
#' @return Numeric value.
#' @export
#'
#' @examples
decimal_to_permille <- function(delta) {
  if(!is.numeric(delta)) {
    stop("Expecting a numeric delta value.")
  }
  out <- delta / 1000
  return(out)
}


