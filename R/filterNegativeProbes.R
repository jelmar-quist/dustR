#' Filtering of negative control probes
#' @description Inspects the relationship between the number of sequencing reads and the abundance of each negative control probe.
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet().
#'
#' @return A readNanoStringGeoMxSet object.
#' @export
#'
#' @examples
#' filterNegativeProbes(eSet)
filterNegativeProbes <- function(eSet = eSet) {

  # Retrieve all negative control probes from fData
  negativeProbes <- rownames(eSet)[which(fData(eSet)$CodeClass == "Negative")]
  negativeProbes_list <- NULL

  # For each negative probe, retrieve data
  for (RTS in negativeProbes) {
    negativeProbes_list[[RTS]] <- data.frame(DCC = colnames(eSet),
                                             Raw = sData(eSet)$Raw,
                                             Count = exprs(eSet)[RTS,],
                                             row.names = NULL)
  }

  # Retrieve slope for each negative probe
  negativeProbes_list <- lapply(negativeProbes_list, getSlope)

  negativeProbes_data <- data.frame(RTS = names(negativeProbes_list),
                                    Slope = t(data.frame(negativeProbes_list)),
                                    row.names = NULL)

  # Retrieve flag for each probe
  negativeProbes_data$Flag <- setNegativeProbe(negativeProbes_data$Slope)

  # Remove poor performing negative control probes from eSet
  eSet_filtered <- eSet[!rownames(fData(eSet)) %in% negativeProbes_data[negativeProbes_data$Flag == "FAIL", "RTS"],]

  message("Removed ", length(which(negativeProbes_data$Flag == "FAIL")), " out of ",
      nrow(negativeProbes_data), " negative control probes due to poor performance.")

  return(eSet_filtered)

}
