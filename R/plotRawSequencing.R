#' Plot Area and Sequencing Reads
#' @description Plots the relationship between the segment area and the number of sequencing reads. Poor performing segments (from checkSequencing()), are indicated in the plot.
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet().
#' @param Area The column containing the area size.
#' @param Raw The column containing the number of raw sequencing reads.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' plotRawSequencing(eSet)
plotRawSequencing <- function(eSet = eSet, Area = "Area", Raw = "Raw") {

  eSet$Raw <- sData(eSet)$Raw

  plotRawSequencing <- ggplot(pData(eSet), aes(x = Area+1, y = Raw, fill = Segment, colour = AreaReads)) +
    geom_point(shape = 21, size = 4, stroke = 1) +
    geom_smooth(method = "glm",  colour = "black", se = FALSE, show.legend = FALSE, aes(fill = NULL, colour = NULL)) +
    scale_colour_manual(values = c(rgb(0, 0, 0, alpha = 0), "black")) +
    geom_vline(xintercept = 5000+1, linetype = "dashed") +
    geom_hline(yintercept = 1000+1, linetype = "dashed") +
    xlab("Segment area (in um)") + ylab("Number of raw sequencing reads") +
    scale_x_log10(labels = label_comma()) +
    scale_y_log10(labels = label_comma()) +
    theme_classic() +
    theme(axis.text = element_text(colour = "black")) +
    guides(fill = guide_legend(override.aes = list(shape = 21, colour = rgb(0, 0, 0, alpha = 0))),
           colour = "none")

  return(plotRawSequencing)

}
