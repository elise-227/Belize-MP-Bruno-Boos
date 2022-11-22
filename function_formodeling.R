#clean space
rm(list=ls())

#load packages
#packages
require(tidyverse)
require(dplyr)
require(sf)
require(leaflet)
require(mapview)
require(readxl)
require(lubridate)
require(readxl)
library(unmarked)
library(AICcmodavg)
library(camtrapR)
library(ubms)


##load dfs
load("dat.RData")
load("camop.RData")
load("covs_all.RData")







modlist <- list()
models <- function(species) {
  
  detect_hist <- detectionHistory(recordTable = dat, species = species, 
                                     camOp = cam_op, output = "binary", stationCol = "site", 
                                     speciesCol = "Species", day1 ="station", 
                                     recordDateTimeCol = "date_time_obs", 
                                     recordDateTimeFormat =  "%Y-%m-%d %H.%M", 
                                     timeZone = "UTC", occasionLength = 1, 
                                     includeEffort = TRUE, scaleEffort = FALSE, 
                                     writecsv = FALSE)
  
  unmarkedFrame <- unmarkedFrameOccu(y = detect_hist_GF$detection_history, siteCovs = site_covs2)
  
  unmarkedFrame@siteCovs$Canopy_Height_m <- scale(unmarkedFrame@siteCovs$Canopy_Height_m)
  unmarkedFrame@siteCovs$LatLong <- scale(unmarkedFrame@siteCovs$LatLong)
  unmarkedFrame@siteCovs$mean_bioma <- scale(unmarkedFrame@siteCovs$mean_bioma)
}