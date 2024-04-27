#' Plot Nuclei versus Area
#' @description Plots the relationship between the number of nuclei and the area size to identify poor performing segments.
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet().
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' plotNuclei(eSet)
plotNuclei <- function(eSet = eSet) {

  plotNucleiArea <- ggplot(pData(eSet), aes(x = Nuclei+1, y = Area+1, fill = Segment, colour = NucleiArea)) +
    geom_point(shape = 21, size = 4, stroke = 1) +
    geom_smooth(method = "glm",  colour = "black", se = FALSE, show.legend = FALSE, aes(fill = NULL, colour = NULL)) +
    scale_colour_manual(values = c(rgb(0, 0, 0, alpha = 0), "black")) +
    geom_vline(xintercept = 100+1, linetype = "dashed") +
    geom_hline(yintercept = 5000+1, linetype = "dashed") +
    xlab("Number of nuclei") + ylab("Segment area (in um)") +
    scale_x_log10(labels = label_comma()) +
    scale_y_log10(labels = label_comma()) +
    theme_classic() +
    theme(axis.text = element_text(colour = "black")) +
    guides(fill = guide_legend(override.aes = list(shape = 21, colour = rgb(0, 0, 0, alpha = 0))),
           colour = "none")

  return(plotNucleiArea)

}
