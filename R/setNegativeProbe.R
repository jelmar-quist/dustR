#' Flags a set of negative control probes
#' @description Identifies negative control probes of poor performance by assessing the relationship between its counts and the number of raw sequencing reads.
#' @param negativeProbes_data A data.frame of RTS and Slope
#'
#' @return A flag ("PASS" or "FAIL") for each negative control probe.
#' @export
#'
#' @examples
#' setNegativeProbe(negativeProbes_data)
setNegativeProbe <- function(negativeProbes_data) {
  negativeProbes_clusters <- Mclust(negativeProbes_data)
  negativeProbes_clusters <- factor(negativeProbes_clusters$classification)

  levels(negativeProbes_clusters) <- c("PASS", rep("FAIL", length(unique(negativeProbes_clusters))-1))

  return(negativeProbes_clusters)
}
