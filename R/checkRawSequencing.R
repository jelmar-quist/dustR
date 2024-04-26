#' Relationship between segment size and number of sequencing reads
#' @description The relationship between area size and the number of sequencing reads across the experiment is assessed and any segments which deviating from this relationship are flagged.
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet().
#' @param removeZero If TRUE, will discard segments where either the number of nuclei or the area size is 0.
#' @param Area The column containing the area size.
#' @param Raw The column containing the number of raw sequencing reads.
#'
#' @return A readNanoStringGeoMxSet object.
#' @export
#'
#' @examples
#' checkRawSequencing(eSet)
checkRawSequencing <- function(eSet = eSet, removeZero = TRUE, Area = "Area", Raw = "Raw") {

  eSet$Raw <- sData(eSet)$Raw

  # Remove segments with NA Raw
  withNA <- which(is.na(eSet$Raw))

  if ( length(withNA) != 0 ) {
    message("Removed", length(withNA), " segments without sequencing reads.")
    eSet <- eSet[,-withNA]
    #dim(eSet[,-withNA])
  }


  # Remove segments with 0 Nuclei or Area
  withZero <- which(eSet$Raw == 0)

  if ( length(withZero) != 0 ) {
    message("Removed", length(withZero), " segments with 0 sequencing reads.")
    eSet <- eSet[,-withZero]
    #dim(eSet[,-withNA])
  }

  # Create model between Nuclei and Area
  model <- lmrob(log10(Raw) ~ log10(Area+1), data = pData(eSet), setting = "KS2014")
  cutoff <- summary(model)$control$eps.outlier
  pData(eSet)$AreaReads <- ifelse(summary(model)$rweights <= cutoff, TRUE, FALSE)

  message(length(which(summary(model)$rweights <= cutoff)), " out of ", ncol(eSet),
          " segments had less sequencing reads than expected and were flagged.")

  pData(eSet) <- subset(pData(eSet), select = -Raw)
  return(eSet)
}
