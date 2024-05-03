## ----message = FALSE, warning = FALSE, include = FALSE------------------------
knitr::opts_chunk$set(echo = TRUE, message = TRUE, warning = FALSE, dpi = 80, out.width = "80%")

## ----eval = FALSE-------------------------------------------------------------
#  if (!require("devtools", quietly = TRUE))
#      install.packages("devtools")
#  
#  devtools::install_github("jelmar-quist/dustR")

## ----setup, echo = TRUE, results = "hide", message = FALSE--------------------
library(dustR)
library(GeomxTools)
library(scales)
library(robustbase)
library(mclust)
library(reshape)

## ----loadData-----------------------------------------------------------------
load("/Users/k1217281/Documents/GeoMX/EllieAlberts/materials/v2/eset.RData")

## ----checkNTCs----------------------------------------------------------------
checkNTC(eSet)

## ----plotNTC------------------------------------------------------------------
plotNTC(eSet)

## ----checkNTCcounts-----------------------------------------------------------
checkNTCcounts(eSet)

## ----removeNTCs---------------------------------------------------------------
eSet <- eSet[,grep("A01", colnames(eSet), invert = TRUE)]

## ----plotNegativeProbes-------------------------------------------------------
plotNegativeProbes(eSet)

## ----filterNegativeProbes-----------------------------------------------------
eSet <- filterNegativeProbes(eSet)

## ----processData--------------------------------------------------------------
source("/Users/k1217281/Documents/resources/GeoMx/calculateNegativeProbe.R")
eSet <- shiftCountsOne(eSet)
eSet <- calculateNegativeProbe(eSet)

## ----checkNuclei--------------------------------------------------------------
eSet <- checkNuclei(eSet)

## ----plotNuclei---------------------------------------------------------------
plotNuclei(eSet)

## ----checkRawSequencing-------------------------------------------------------
eSet <- checkRawSequencing(eSet)

## ----plotRawSequencing--------------------------------------------------------
plotRawSequencing(eSet)

## ----checkSaturation----------------------------------------------------------
eSet <- checkSaturation(eSet)

## ----plotSaturation-----------------------------------------------------------
plotSaturation(eSet)

## ----plotGeoMeanNegProbes-----------------------------------------------------
plotGeoMeanNegProbes(eSet)

## ----checkGeoMeanNegProbes----------------------------------------------------
eSet <- checkGeoMeanNegProbes(eSet)

## ----plotFilterSegments-------------------------------------------------------
plotFilterSegments(eSet)

## ----filterSegments-----------------------------------------------------------
eSet <- filterSegments(eSet)

## ----createPseudobulk_bySlide-------------------------------------------------
load("/Users/k1217281/Documents/GeoMX/EllieAlberts/materials/v2/eset.RData")
eSet_pseudobulk <- createPseudobulk(eSet, id.vars = "Slide Name")
ncol(eSet)
ncol(eSet_pseudobulk)

## ----createPseudobulk_byROI---------------------------------------------------
load("/Users/k1217281/Documents/GeoMX/EllieAlberts/materials/v2/eset.RData")
eSet_pseudobulk <- createPseudobulk(eSet, id.vars = "Slide Name", bySlide = FALSE)
ncol(eSet)
ncol(eSet_pseudobulk)

## ----createSTlist-------------------------------------------------------------
require(spatialGE)

load("/Users/k1217281/Documents/GeoMX/LEAP_SA_v3/materials/SA_ReadInOnly.RData")
eSet <- eSet[,!is.na(eSet$Segment)]
eSet <- eSet[,eSet$Segment == "PanCK+"]

eSet_STlist <- createSTlist(eSet, id.vars = "LEAP_ID")

