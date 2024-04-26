#' Plot of segments of poor quality
#' @description A summary plot depicting the various filtering criteria. Some of these criteria are likely interconnected.
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet().
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' plotFilterSegments(eSet)
plotFilterSegments <- function(eSet = eSet) {

  data_filtering <- data.frame(DCC = colnames(eSet),
                               NucleiArea = eSet$NucleiArea,
                               AreaReads = eSet$AreaReads,
                               SeqSaturation = eSet$SeqSaturation,
                               NegProbes = eSet$NegProbes)

  data_filtering_count <- apply(subset(data_filtering, select = -DCC), 1, sum)
  names(data_filtering_count) <- data_filtering$DCC

  data_filtering <- data_filtering[data_filtering_count > 0,]
  data_filtering_count <- data_filtering_count[data_filtering_count > 0]

  data_filtering <- melt(data_filtering, id.vars = "DCC")
  data_filtering$DCC <- factor(data_filtering$DCC, levels = names(data_filtering_count[order(data_filtering_count)]))
  data_filtering$variable <- factor(data_filtering$variable, levels = rev(c("NucleiArea", "AreaReads",
                                                                            "SeqSaturation", "NegProbes")))

  plotFilterSegments <- ggplot(data_filtering, aes(x = DCC, y = variable, fill = value)) +
    geom_tile() +
    scale_fill_manual(values = c("gray80", "firebrick2")) +
    theme_classic() +
    theme(axis.text = element_text(colour = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")

  return(plotFilterSegments)

}
