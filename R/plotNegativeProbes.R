#' Plot  of negative control probe performance.
#' @description Plots the relationship between the number of sequencing reads and the abundance of each negative control probe.
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet().
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' plotNegativeProbes(eSet)
plotNegativeProbes <- function(eSet = eSet) {

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
  negativeProbes_data$Colour <- ifelse(negativeProbes_data$Flag == "PASS", "gray80",
                                       ifelse(negativeProbes_data$Flag == "FAIL", "firebrick2", "black"))

  # Plot density
  plot_NegativeProbes <- ggplot(negativeProbes_data, aes(x = Slope)) +
    geom_density(colour = "black") +
    annotate("segment", x = negativeProbes_data$Slope, xend = negativeProbes_data$Slope,
             y = 0-max(density(negativeProbes_data$Slope)$y)*0.02,
             yend = max(density(negativeProbes_data$Slope)$y)*0.02,
             colour = negativeProbes_data$Colour) +
    coord_cartesian(clip="off") +
    scale_x_continuous(labels = label_comma()) +
    theme_classic() +
    theme(axis.text = element_text(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("Slope") + ylab("Density")

  return(plot_NegativeProbes)

}



