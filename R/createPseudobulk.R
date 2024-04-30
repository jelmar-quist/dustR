#' Generates pseudobulk data
#' @description Creates a pseudobulk NanoStringGeoMxSet object by aggregating gene counts from all segments belonging to the same sample.
#' @param eSet A NanoStringGeoMxSet object.
#' @param id.vars Variable corresponding to the column name used for melting. If not suplied, pseudobulk per ROI will be generated instead.
#'
#' @return A readNanoStringGeoMxSet object.
#' @export
#'
#' @examples
#' createPseudobulk(eSet, id.vars = "SampleID")
createPseudobulk <- function(eSet, id.vars, bySlide = TRUE) {

  eSet <- eSet[,grep("A01", colnames(eSet), invert = TRUE)]

  if ( bySlide == TRUE ) {

    # Create pseudobulk for each unique value in id.vars
    eSet_pseudobulk <- eSet

    eSet_pseudobulk$Raw_pseudo <- NA
    eSet_pseudobulk$Aligned_pseudo <- NA
    eSet_pseudobulk$DeduplicatedReads_pseudo <- NA

    # For each unique id.vars
    for ( id in unique(pData(eSet)[,id.vars]) ) {

      # Subset eSet
      eSet_id <-  eSet[,pData(eSet)[,id.vars] == id]

      # Only proceed if there is more than 1 ROI
      if( ncol(eSet_id) > 1 ) {

        # Sum counts
        exprs(eSet_pseudobulk)[, colnames(eSet_id)[1]] <- rowSums(exprs(eSet_id)[, colnames(eSet_id)[1:ncol(eSet_id)]])

        # Collate segment features
        pData(eSet_pseudobulk)[colnames(eSet_id)[1], "Segment"] <- "Pseudobulk"
        pData(eSet_pseudobulk)[colnames(eSet_id)[1], "Nuclei"] <- sum(eSet_id$Nuclei)
        pData(eSet_pseudobulk)[colnames(eSet_id)[1], "Area"] <- sum(eSet_id$Area)

        # Collate sequencing
        pData(eSet_pseudobulk)[colnames(eSet_id)[1], "Raw_pseudo"] <- sum(sData(eSet_id)$Raw)
        pData(eSet_pseudobulk)[colnames(eSet_id)[1], "Aligned_pseudo"] <- sum(sData(eSet_id)$Aligned)
        pData(eSet_pseudobulk)[colnames(eSet_id)[1], "DeduplicatedReads_pseudo"] <- sum(sData(eSet_id)$DeduplicatedReads)

        # Remove merge segments (except first)
        dim(eSet_pseudobulk)
        eSet_pseudobulk <- eSet_pseudobulk[,!colnames(eSet_pseudobulk) %in% colnames(eSet_id)[2:ncol(eSet_id)]]
        dim(eSet_pseudobulk)

      }
    }

    return(eSet_pseudobulk)

  } else if ( bySlide == FALSE ) {

    # Create pseudobulk for each unique value in id.vars
    eSet$id_vars_roi <- paste0(pData(eSet)[,id.vars], "_ROI_", pData(eSet)[,"Roi"])
    eSet_pseudobulk <- eSet
    eSet_pseudobulk$Raw_pseudo <- NA
    eSet_pseudobulk$Aligned_pseudo <- NA
    eSet_pseudobulk$DeduplicatedReads_pseudo <- NA

    # For each unique id.vars
    for ( id in unique(pData(eSet)[,"id_vars_roi"]) ) {

      # Subset eSet
      eSet_id <-  eSet[,pData(eSet)[,"id_vars_roi"] == id]

      # Only proceed if there is more than 1 ROI
      if( ncol(eSet_id) > 1 ) {

        # Sum counts
        exprs(eSet_pseudobulk)[, colnames(eSet_id)[1]] <- rowSums(exprs(eSet_id)[, colnames(eSet_id)[1:ncol(eSet_id)]])

        # Collate segment features
        pData(eSet_pseudobulk)[colnames(eSet_id)[1], "Segment"] <- paste0(eSet_id$Segment, collapse = "_")
        pData(eSet_pseudobulk)[colnames(eSet_id)[1], "Nuclei"] <- sum(eSet_id$Nuclei)
        pData(eSet_pseudobulk)[colnames(eSet_id)[1], "Area"] <- sum(eSet_id$Area)

        # Collate sequencing
        pData(eSet_pseudobulk)[colnames(eSet_id)[1], "Raw_pseudo"] <- sum(sData(eSet_id)$Raw)
        pData(eSet_pseudobulk)[colnames(eSet_id)[1], "Aligned_pseudo"] <- sum(sData(eSet_id)$Aligned)
        pData(eSet_pseudobulk)[colnames(eSet_id)[1], "DeduplicatedReads_pseudo"] <- sum(sData(eSet_id)$DeduplicatedReads)

        # Remove merge segments (except first)
        dim(eSet_pseudobulk)
        eSet_pseudobulk <- eSet_pseudobulk[,!colnames(eSet_pseudobulk) %in% colnames(eSet_id)[2:ncol(eSet_id)]]
        dim(eSet_pseudobulk)

      }
    }

    return(eSet_pseudobulk)

  }
}
