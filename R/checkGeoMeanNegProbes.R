#' Check Negative Control Probes
#' @description Geometric mean of the negative control probes determines the limit of quantification and subsequentially the gene detection rate. Segments with a low geometric mean are flagged.
#'
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet().
#' @param minNegGeoMean Segments below this threshold are flagged. Default is 1.
#' @param NegGeoMean The column reporting the geometric mean of the negative control probes.
#'
#' @return A readNanoStringGeoMxSet object.
#' @export
#'
#' @examples
#' checkGeoMeanNegProbes(eSet)
checkGeoMeanNegProbes <- function(eSet = eSet, minNegGeoMean = 1, NegGeoMean = "NegGeoMean") {

  eSet$NegProbes <- ifelse(eSet$NegGeoMean <= minNegGeoMean, TRUE, FALSE)

  message("The average gene detection rate in segments with a NegGeoMean <= ", minNegGeoMean, " is ",
      paste0(round(mean(eSet$GeneDetectionRate[eSet$NegProbes == TRUE]) * 100, 2), "%"))
  message("The average gene detection rate in segments with a NegGeoMean > ", minNegGeoMean, " is ",
          paste0(round(mean(eSet$GeneDetectionRate[eSet$NegProbes == FALSE]) * 100, 2), "%"))
  message(length(which(eSet$NegGeoMean <= minNegGeoMean)), " out of ", ncol(eSet),
          "were flagged due to a low geometric mean of the negative control probes.")

  return(eSet)

}
