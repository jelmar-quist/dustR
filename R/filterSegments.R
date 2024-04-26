#' Filtering of segments
#' @description Function that removes all segments flagged during the quality control process.
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet().
#'
#' @return A readNanoStringGeoMxSet object.
#' @export
#'
#' @examples
#' filterSegments(eSet)
filterSegments <- function(eSet = eSet) {

  eSet_filtered <- eSet[, !(eSet$NucleiArea == TRUE |
                              eSet$AreaReads == TRUE |
                              eSet$SeqSaturation == TRUE |
                              eSet$NegProbes == TRUE)]

  message("Removed", (ncol(eSet)-ncol(eSet_filtered)), " out of ", ncol(eSet), "segments that were flagged.")

  return(eSet_filtered)

}
