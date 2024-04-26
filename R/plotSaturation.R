#' Barplot of sequencing saturation
#' @description A barplot of sequencing saturation across all segments.
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet().
#' @param minSaturation Segments below this threshold are flagged. Default is 50.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' plotSaturation(eSet)
plotSaturation <- function(eSet, minSaturation = 50) {

  data_SeqSaturation <- data.frame(DCC = colnames(eSet),
                                   Segment = eSet$Segment,
                                   Saturation = eSet$Saturation)

  data_SeqSaturation$DCC <- factor(data_SeqSaturation$DCC,
                                   levels = data_SeqSaturation$DCC[order(data_SeqSaturation$Saturation)])

  plotSaturation <- ggplot(data_SeqSaturation, aes(x = DCC, y = Saturation, fill = Segment)) +
    geom_bar(stat = "identity") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    geom_hline(yintercept = 50, linetype = "dashed") +
    geom_hline(yintercept = median(data_SeqSaturation$Saturation), linetype = "solid") +
    theme_classic() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab("Segments") + ylab("Sequencing saturation (%)")

  return(plotSaturation)

}
