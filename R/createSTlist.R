#' Create an STlist object
#' @description Creates an STlist object for downstream analysis with `spatialGE`
#' @param eSet A NanoStringGeoMxSet object.
#' @param id.vars Variable corresponding to the column name used for melting.
#'
#' @return An STlist object.
#' @export
#'
#' @examples
#' createSTlist(eSet, id.vars = "SampleID")
createSTlist <- function(eSet, id.vars) {

  eSet_counts <- list()
  eSet_spotcoords <- list()

  for ( id in unique(pData(eSet)[,id.vars]) ) {

    # Subset eSet
    eSet_id <-  eSet[,pData(eSet)[,id.vars] == id]

    # Create data.frame with TargetName instead of RTS
    eSet_id_counts <- data.frame(genename = fData(eSet_id)$TargetName,
                              exprs(eSet_id),
                              row.names = NULL)

    # Convert sample names to ROI
    colnames(eSet_id_counts) <- c("genename", paste0(id, "_ROI", eSet_id$Roi))

    # Assign to list
    eSet_counts[[id]] <- eSet_id_counts

    # Create data.frame for coordinates
    eSet_id_spotcoords <- data.frame(ROIID = paste0(id, "_ROI", eSet_id$Roi),
                                 Y = eSet_id$ROI.Coordinate.Y,
                                 X = eSet_id$ROI.Coordinate.X,
                                 row.names = NULL)
    # Assign to list
    eSet_spotcoords[[id]] <- eSet_id_spotcoords

  }

  # Create STlist
  eSet_STlist <- STlist(rnacounts = eSet_counts, spotcoords = eSet_spotcoords, samples = NULL)

  # Update counts
  eSet_STlist@tr_counts = eSet_STlist@counts

  # Create gene meta

  for ( id in eSet_STlist@sample_meta$sample_name ) {

    eSet_STlist@gene_meta[[id]] <- data.frame(gene = rownames(eSet_STlist@tr_counts[[id]]),
                                                  gene_mean = apply(eSet_STlist@tr_counts[[id]], 1, mean),
                                                  gene_stdevs = apply(eSet_STlist@tr_counts[[id]], 1, sd),
                                                  row.names = NULL)
  }

  return(eSet_STlist)

}
