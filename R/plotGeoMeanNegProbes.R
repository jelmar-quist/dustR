#' Plot geometric mean of the negative control probes.
#' @description A barplot of the geometric mean of the negative control probes by segment.
#'
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet().
#' @param minNegGeoMean Segments below this threshold are flagged. Default is 1.
#' @param NegGeoMean The column reporting the geometric mean of the negative control probes.
#' @param Segment Name of column containing segment information (e.g. PanCK+, CD45, ...).
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' plotGeoMeanNegProbes(eSet)
plotGeoMeanNegProbes <- function(eSet, minNegGeoMean = 1, NegGeoMean = "NegGeoMean", Segment = "Segment") {

  data_NegativeProbes <- data.frame(DCC = colnames(eSet),
                                    Segment = eSet$Segment,
                                    GeoMean = eSet$NegGeoMean)
  data_NegativeProbes$DCC <- factor(data_NegativeProbes$DCC,
                                    levels = data_NegativeProbes$DCC[order(data_NegativeProbes$GeoMean)])

  plotNegativeProbes <- ggplot(data_NegativeProbes, aes(x = DCC, y = GeoMean, fill = Segment)) +
    geom_bar(stat = "identity") +
    scale_y_log10() +
    theme_classic() +
    theme(axis.text = element_text(colour = "black"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("Segments") + ylab("Geometric mean of negative control probes")

  return(plotNegativeProbes)

}
