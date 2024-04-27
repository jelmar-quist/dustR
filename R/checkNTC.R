#' No Template Control assessment
#' @description A No Template Control (NTC) is included in each sequencing plate (well A01) to detect contamination of the sequencing library. NTCs with more than 1,000 sequencing reads could indicate contamination and should be examined further.
#' @param eSet A NanoStringGeoMxSet object.
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' checkNTC(eSet)
checkNTC <- function(eSet) {

  # Identify NTC containing >= 1000 sequencing reads
  eSet_NTC <- eSet[,grep("A01", fixed = TRUE, colnames(eSet))]
  NTC <- ifelse(sData(eSet_NTC)$Raw < 1000, FALSE, TRUE)

  # Set flag for segments from same plate
  NTC_toFlag <- colnames(eSet_NTC)[which(NTC == TRUE)]
  eSet$NTC <- FALSE
  eSet$NTC[grep(paste(substr(NTC_toFlag, 13, 19), collapse="|"), colnames(eSet))] <- TRUE

  message("Identified ", length(which(NTC == TRUE)), " out of ", ncol(eSet_NTC), " NTCs with >= 1000 sequencing reads.")
  message(length(which(eSet$NTC == TRUE)), " out of ", ncol(eSet), " segments may be at risk, please run checkNTCcounts() before proceeding.")

}
