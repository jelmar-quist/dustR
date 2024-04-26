#' Check NTC count uniformity
#' @description Plots a histogram of counts for each NTC to assess uniformity.
#' @param eSet A NanoStringGeoMxSet acquired via readNanoStringGeoMxSet().
#' @param Raw Name of the column containing the number of raw sequencing reads.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' checkNTCcounts(eSet)
checkNTCcounts <- function(eSet, Raw = "Raw") {

  eSet_NTC <- eSet[,grep("A01", fixed = TRUE, colnames(eSet))]

  NTC <- data.frame(DCC = colnames(eSet_NTC),
                    Raw = sData(eSet_NTC)$Raw,
                    NTC = ifelse(sData(eSet_NTC)$Raw < 1000, FALSE,
                                 ifelse(sData(eSet_NTC)$Raw >= 10000, TRUE, NA)))
  NTC <- NTC[which(NTC$NTC == TRUE),]
  NTC$DCC <- factor(NTC$DCC, levels = NTC$DCC[order(NTC$Raw)])

  eSet_NTC <- eSet_NTC[,colnames(eSet_NTC) %in% NTC$DCC]



  NTC_counts <- data.frame(DCC = colnames(eSet_NTC),
                           t(exprs(eSet_NTC)))
  NTC_counts <- melt(NTC_counts, id.vars = "DCC")
  NTC_counts$DCC <- factor(NTC_counts$DCC, levels = NTC$DCC[order(NTC$Raw)])


  # ggplot(NTC_counts, aes(x = DCC, y = value+1)) +
  #   geom_boxplot() +
  #   theme_classic() +
  #   theme(axis.text = element_text(colour = "black"),
  #         axis.title.x = element_blank(),
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
  #         legend.position = "none") +
  #   scale_y_log10(breaks = c(1, 10, 100, 1000, 10000)) +
  #   ylab("Counts (log10)")

  plotNTCcounts <- ggplot(NTC_counts, aes(x = value+1)) +
    geom_histogram(binwidth = 1) +
    facet_wrap(.~DCC, scales = "free") +
    theme_classic() +
    theme(axis.text = element_text(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none") +
    xlab("Number of sequencing reads") + ylab("Count")

  return(plotNTCcounts)

}
