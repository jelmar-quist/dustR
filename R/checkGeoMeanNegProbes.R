#' Assessment of NegGeoMean
#' @description The geometric mean of the negative control probes (NegGeoMean) is used to distinguish signal from noise and determines the quantifiable limit of gene expression. Segments with a very low NegGeoMean often exhibit low gene detection rates.
#' @param eSet A NanoStringGeoMxSet object.
#' @param minNegGeoMean Threshold to define the minimum required NegGeoMean. Default is 1.
#'
#' @return A readNanoStringGeoMxSet object.
#' @export
#'
#' @examples
#' checkGeoMeanNegProbes(eSet)
checkGeoMeanNegProbes <- function(eSet, minNegGeoMean = 1) {

  eSet$NegProbes <- ifelse(eSet$NegGeoMean <= minNegGeoMean, TRUE, FALSE)

  message("The average gene detection rate in segments with a NegGeoMean <= ", minNegGeoMean, " is ",
      paste0(round(mean(eSet$GeneDetectionRate[eSet$NegProbes == TRUE]) * 100, 2), "%"))
  message("The average gene detection rate in segments with a NegGeoMean > ", minNegGeoMean, " is ",
          paste0(round(mean(eSet$GeneDetectionRate[eSet$NegProbes == FALSE]) * 100, 2), "%"))
  message(length(which(eSet$NegGeoMean <= minNegGeoMean)), " out of ", ncol(eSet),
          "were flagged due to a low geometric mean of the negative control probes.")

  return(eSet)

}
