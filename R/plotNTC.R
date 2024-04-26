#' Plot the number of raw sequencing read for each NTC
#' @description Plots the number of sequencing reads detected in each No Template Control (NTC).
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet().
#' @param Raw Name of the column containing the number of raw sequencing reads.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' plotNTC(eSet)
plotNTC <- function(eSet = eSet, Raw = "Raw") {

  eSet_NTC <- eSet[,grep("A01", fixed = TRUE, colnames(eSet))]

  NTC <- data.frame(DCC = colnames(eSet_NTC),
                    Raw = sData(eSet_NTC)$Raw,
                    Total = colSums(exprs(eSet_NTC)))

  NTC$NTC <- ifelse(NTC$Raw < 1000, FALSE,
                    ifelse(NTC$Raw >= 1000, TRUE, NA))
  NTC$NTC <- factor(NTC$NTC, levels = c(FALSE, TRUE))
  NTC$DCC <- factor(NTC$DCC, levels = NTC$DCC[order(NTC$Raw)])

  plotNTC <- ggplot(NTC, aes(x = DCC, y = Raw, fill = NTC)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("gray80", "firebrick2")) +
    geom_hline(yintercept = 1000, linetype = "dashed") +
    geom_hline(yintercept = 10000, linetype = "dashed") +
    theme_classic() +
    theme(axis.text = element_text(colour = "black"),
          axis.title.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "none") +
    scale_y_log10(labels = label_comma()) +
    ylab("Number of raw sequencing reads")

  return(plotNTC)

}
