#' No Template Control count uniformity
#' @description In No Template Controls (NTCs) with more than 1,000 sequencing reads, counts are expected to be uniformly distributed (i.e. detected at a similar abundances). This is assessed by comparing the gene count distribution of each NTC with a uniform distribution of comparable parameters using a Kolmogorov–Smirnov test.
#' @param eSet A NanoStringGeoMxSet object.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' checkNTCcounts(eSet)
checkNTCcounts <- function(eSet) {

  # Retrieve NTCs
  eSet_NTC <- eSet[,grep("A01", fixed = TRUE, colnames(eSet))]

  # Create data.frame to identify poor performing NTCs
  NTC <- data.frame(DCC = colnames(eSet_NTC),
                    Raw = sData(eSet_NTC)$Raw,
                    NTC = ifelse(sData(eSet_NTC)$Raw < 1000, FALSE,
                                 ifelse(sData(eSet_NTC)$Raw >= 10000, TRUE, NA)))
  NTC <- NTC[which(NTC$NTC == TRUE),]

  # Only check poor performing NTCs and remove negative control probes
  eSet_NTC <- eSet_NTC[fData(eSet_NTC)$CodeClass == "Endogenous", colnames(eSet_NTC) %in% NTC$DCC]

  # Retrieve gene counts for each NTC
  NTC_counts <- data.frame(t(exprs(eSet_NTC)),
                           row.names = colnames(eSet_NTC))

  generateRunIf <- function(x, report = "punif") {

    #x_runif <- ks.test(x, "punif")
    x_runif <- round(runif(length(x), min = round(min(x)), max = round(max(x))))
    x_ks <- ks.test(x, x_runif)
    #x_ks <- ks.test(x, "punif", min = round(min(x)), max = round(max(x)))

    if( report == "pvalue") {
      return(x_ks$p.value)
    }

    if ( report == "punif" ) {
      return(x_ks$data$y)
    }
  }

  # Generate runif for each segment
  NTC_runif <- data.frame(t(apply(NTC_counts, 1, generateRunIf, report = "punif")),
                          row.names = paste0(colnames(eSet_NTC), "_runif"))
  NTC_runif_pvalue <- data.frame(apply(NTC_counts, 1, generateRunIf, report = "pvalue"))
  NTC_runif_pvalue <- data.frame(DCC = rownames(NTC_runif_pvalue),
                                 P = paste0("P = ", as.numeric(NTC_runif_pvalue[,1])),
                                 Data = "Segment",
                                 row.names = NULL)

  # Convert NTC_counts
  NTC_counts <- melt(data.frame(DCC = row.names(NTC_counts),
                           NTC_counts,
                           row.names = NULL), id.vars = "DCC")
  NTC_counts$Data = "Segment"

  # Convert NTC_runif
  NTC_runif <- melt(data.frame(DCC = gsub("_runif", "", row.names(NTC_runif)),
                               NTC_runif,
                               row.names = NULL), id.vars = "DCC")
  NTC_runif$Data = "runif"

  # Combine
  NTC_plot_data <- rbind(NTC_counts, NTC_runif)

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

  plotNTCcounts <- ggplot(NTC_plot_data, aes(x = value, fill = Data)) +
    geom_histogram(binwidth = 1, alpha = 0.5) +
    scale_fill_manual(values = c("gray80", "black")) +
    facet_wrap(.~DCC, scales = "free") +
    theme_classic() +
    theme(axis.text = element_text(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none") +
    xlab("Number of sequencing reads") + ylab("Count") +
    geom_text(x = Inf, y = Inf, vjust = 1, hjust = 1,
              aes(label = P), data = NTC_runif_pvalue)

  return(plotNTCcounts)

}
