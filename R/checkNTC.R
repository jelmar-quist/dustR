#' Checks the performance of NTCs
#' @description Checks the performance of the No Template Control (NTC) in a GeoMx DSP experiment.
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet().
#' @param Raw Name of the column containing the number of raw sequencing reads.
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' checkNTC(eSet)
checkNTC <- function(eSet = eSet, Raw = "Raw") {

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
