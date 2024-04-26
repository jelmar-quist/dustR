#' Assessment of the relationship between Nuclei and Area
#' @description Assesses the relationship between the number of nuclei and the area size across with experiment. Segments which deviate from this relationship are flagged.
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet().
#' @param removeZero If TRUE, will discard segments where either the number of nuclei or the area size is 0.
#' @param Nuclei The column containing the nuclei counts.
#' @param Area The column containing the area size.
#'
#' @return A readNanoStringGeoMxSet object.
#' @export
#'
#' @examples
#' checkNuclei(eSet)
checkNuclei <- function(eSet = eSet, removeZero = TRUE, Nuclei = "Nuclei", Area = "Area") {

  # Remove segments with NA Nuclei or Area
  withNA <- which(is.na(eSet$Nuclei) | is.na(eSet$Area))

  if ( length(withNA) != 0 ) {
    message("Removed ", length(withNA), " segments without nuclei or area.")
    eSet <- eSet[,-withNA]
    #dim(eSet[,-withNA])
  }


  # Remove segments with 0 Nuclei or Area
  withZero <- which(eSet$Nuclei == 0 | eSet$Area == 0)

  if ( length(withZero) != 0 ) {
    message("Removed ", length(withNA), " segments with 0 nuclei or area.")
    eSet <- eSet[,-withZero]
    #dim(eSet[,-withNA])
  }

  # Create model between Nuclei and Area
  model <- lmrob(log10(Area+1) ~ log10(Nuclei+1), data = pData(eSet), setting = "KS2014")
  cutoff <- summary(model)$control$eps.outlier
  pData(eSet)$NucleiArea <- ifelse(summary(model)$rweights <= cutoff, TRUE, FALSE)

  message(length(which(summary(model)$rweights <= cutoff)), " out of ", ncol(eSet),
          " segments were flagged due to abnormal behavior.")

  return(eSet)

}
