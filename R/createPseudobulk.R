#' Generates pseudobulk data
#' @description Creates a pseudobulk NanoStringGeoMxSet object by aggregating gene counts from all segments belonging to the same sample.
#' @param eSet A NanoStringGeoMxSet object.
#' @param id.vars Variable corresponding to the column name used for melting.
#'
#' @return A readNanoStringGeoMxSet object.
#' @export
#'
#' @examples
#' createPseudobulk(eSet, id.vars = "SampleID")
createPseudobulk <- function(eSet, id.vars) {

  # for (GSTT_ID in unique(pData(eSet)$GSTT_ID)) {
  #
  #   ##  GET PATIENT
  #   tmp.patient <- eSet[,pData(eSet)$GSTT_ID == GSTT_ID]
  #   tmp.patient$Sample_Type
  #
  #   for (Sample_Type in unique(tmp.patient$Sample_Type)) {
  #     tmp.sample <- tmp.patient[,pData(tmp.patient)$Sample_Type == Sample_Type]
  #
  #     ##  ONLY UPDATE IF THERE IS MORE THAN ROI
  #
  #     if(nrow(pData(tmp.sample)) >1) {
  #       ##  SUM DCC COUNTS
  #       exprs(eSet)[,rownames(pData(tmp.sample))[1]] <- rowSums(exprs(eSet)[,rownames(pData(tmp.sample))[1:nrow(pData(tmp.sample))]])
  #
  #       ##  UPDATE Segment, AOI_Area AND Nuclei (INCLUDE ANY OTHER IMPORTANT SEGMENT MEASURES)
  #       pData(eSet)$Segment <- as.character(pData(eSet)$Segment)
  #       pData(eSet)[rownames(pData(tmp.sample))[1],"Segment"] <- "Pseudobulk"
  #       pData(eSet)[rownames(pData(tmp.sample))[1],"AOI_Area"] <- sum(pData(eSet)[rownames(pData(tmp.sample))[1:nrow(pData(tmp.sample))],"AOI_Area"])
  #       pData(eSet)[rownames(pData(tmp.sample))[1],"Nuclei"] <- sum(pData(eSet)[rownames(pData(tmp.sample))[1:nrow(pData(tmp.sample))],"Nuclei"])
  #
  #       ##  UPDATE SEQUENCING
  #       pData(eSet)[rownames(pData(tmp.sample))[1],"DeduplicatedReads_Pseudo"] <- sum(sData(eSet)[rownames(sData(tmp.sample))[1:nrow(sData(tmp.sample))],"DeduplicatedReads"])
  #       pData(eSet)[rownames(pData(tmp.sample))[1],"Aligned_Pseudo"] <- sum(sData(eSet)[rownames(sData(tmp.sample))[1:nrow(sData(tmp.sample))],"Aligned"])
  #
  #       ##  REMOVE AOIS
  #       pData(eSet)["DSP-1001660008694-G-B09.dcc",]
  #       dim(eSet)
  #       eSet <- eSet[,!rownames(pData(eSet)) %in% rownames(pData(tmp.sample))[2:nrow(pData(tmp.sample))]]
  #       pData(eSet)["DSP-1001660008694-G-B09.dcc",]
  #       dim(eSet)
  #     }
  #   }
  # }
  # rm(list=c("GSTT_ID", "Sample_Type", "tmp.patient", "tmp.sample"));gc()
  # dim(eSet)
  # ##18815       62
  #
  # ## SET FACTOR BACK
  # pData(eSet) <- pData(eSet)[,!colnames(pData(eSet)) %in% c("ROI", "Sample_ID", "ROI_Area", "x", "y", "Epithelial_E", "Region_R",
  #                                                           "Immune_Cell_Localisation_L", "Immune_Cell_Population_P", "Immune_Cell_Number_N",
  #                                                           "S", "E", "R", "L", "P", "N", "ROI_Class")]
  # dim(eSet)
  # ##Features  Samples
  # ##18815       62

}
