#' Saturation vapor pressure of air
#'
#' \code{sat_vap} determines the saturation vapor pressure for a given temperature.
#'
#' Saturation vapor pressure is determine by (Allen et al. Allen et al. 1998):
#'
#' \deqn{e_{s} = \frac{0.611 exp(T 17.3)}{T + 237.3}}
#'
#'
#' @param temperature Air temperature (C). \eqn{T}
#'
#' @return The result is in kPa.
#' @export
#'
#' @examples
#'
sat_vap <- function(temperature){
  right <- exp((T * 17.3) / (T + 237.3))
  es <- 0.611 * right
  es
}
