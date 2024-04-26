#' Assessment of sequencing saturation
#' @description Calculates the sequencing saturation for each sample and flags those with very low sequencing saturation.
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet()
#' @param minSaturation Segments below this threshold are flagged. Default is 50.
#' @param DeduplicatedReads The column containing the number of deduplicated reads.
#' @param Aligned The column containing the number of aligned reads.
#'
#' @return A readNanoStringGeoMxSet object.
#' @export
#'
#' @examples
#' checkSaturation(eSet)
checkSaturation <- function(eSet = eSet, minSaturation = 50, DeduplicatedReads = "DeduplicatedReads", Aligned = "Aligned") {

  SeqSaturation <- (1-(sData(eSet)$DeduplicatedReads/sData(eSet)$Aligned))*100
  eSet$Saturation <- SeqSaturation
  eSet$SeqSaturation <- ifelse(eSet$Saturation < minSaturation, TRUE, FALSE)

  message("Removed ", length(which(SeqSaturation < minSaturation)), " out of ", ncol(eSet),
          " segments due to low sequencing saturation.")

  return(eSet)
}
