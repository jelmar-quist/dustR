#' Estimate slope
#' @description Performs a glm() and calculates the slope.
#' @param NegativeProbes_data List of negative control probes containing DCC, Raw and Count
#' '
#' @return Estimated slope.
#' @export
#'
#' @examples
#' getSlope(NegativeProbes_data)
getSlope <- function(NegativeProbes_data) {

  # Perform glm()
  glm_data <- glm(Count~Raw, data = NegativeProbes_data)

  # Get slope
  glm_slope <- as.numeric(coef(glm_data)[2])

  return(glm_slope)
}
