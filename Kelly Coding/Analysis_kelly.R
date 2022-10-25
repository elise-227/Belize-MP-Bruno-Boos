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
#set wd
setwd("~/Belize-MP-Bruno-Boos")

##load dfs
load("obs_data.RData")
load("cam_op.RData")
covs <- read_xlsx("Covariates_cams.xlsx")

cam_op2 <- cam_op

#cam_op2$Date_Place <- as.Date(cam_op2$Date_Place, tryFormats = c("%m-%d-%Y", "%m/%d/%Y"))
#cam_op2$Last_Record_clean <- as.Date(cam_op2$Last_Record_clean, tryFormats = c("%m-%d-%Y", "%m/%d/%Y"))

#make camera_operation frame
cam_op3 <- cameraOperation(cam_op2, stationCol = "site", setupCol = "Date_Place", 
                             retrievalCol = "Last_Record_clean", occasionStartTime = 0 , 
                             dateFormat = "%Y-%m-%d", writecsv = FALSE)


#detection history for gray fox
detect_hist_GF <- detectionHistory(recordTable = dat, species = "Gray Fox", 
                                             camOp = cam_op3, output = "binary", stationCol = "site", 
                                             speciesCol = "Species", day1 ="station", 
                                             recordDateTimeCol = "date_time_obs", 
                                             recordDateTimeFormat =  "%Y-%m-%d %H.%M", 
                                             timeZone = "UTC", occasionLength = 1, 
                                             includeEffort = TRUE, scaleEffort = FALSE, 
                                             writecsv = FALSE)
##lets do some modeling
sitecovs <- dat %>% #filter(dat$Species == "Gray Fox") %>%
  select(site, Easting, Northing, Month, Hour)
sitecovs <- sitecovs[!duplicated(sitecovs$site), ]

#need to join these to get cam IDs right
detect <- as.data.frame(detect_hist_GF$detection_history, row.names = NULL,
                        stringsAsFactors = FALSE)

detect$cams <- row.names(detect) #column of cams

covs_2 <- left_join(detect, covs, by = c("cams" = "CameraID"))
covs_2 <- select(covs_2, 145:244)

#need to join this but not now bc it lowers sites from 256 to 251
#need to add covs: logging, season, month, cam type, time, lat*long, camera
#covs_3 <- left_join(sitecovs, covs_2, by = c("site" = "cams"))


########################################################################
#something wrong#

#remove whats different (IDK which cams it is)
newdata <- all.equal(sitecovs$site, detect$cams)
data_merge <- inner_join(sitecovs, detect, by = "site")

##########################################################################

#check all covs - for some reason some have been converted to factors?
covs_2$cams <- factor(covs_2$cams)
#covs500[,c(28,41,54,67,80,93)] <- lapply(covs500[,c(28,41,54,67,80,93)], as.numeric)


unmarkedFrame <- unmarkedFrameOccu(y = detect_hist_GF$detection_history, siteCovs = covs_2)
summary(unmarkedFrame)


#standardizing variables
unmarkedFrame@siteCovs$Canopy_Height_m <- scale(unmarkedFrame@siteCovs$Canopy_Height_m)

#start modeling
modlist <- list()

fm1 <- occu(formula = ~1 # detection formula first
                                      ~1,# occupancy formula second,
                                      data = unmarkedFrame)

backTransform(fm1, type = "state")
backTransform(fm1, type = "det")

###models with Random effect
#ubms is Bayesian
modlist[["int"]] <- fm2 <- stan_occu(data = unmarkedFrame, formula = ~1 ~1)
model

output <- summary(fm2, "state")
logit <- output$mean[1]
odds <- exp(logit)
prob1 <- odds / (1 + odds)
prob1
#occupancy probability ~ 0.622 or we expect to find gray foxes at ~62% of sites

modlist[["canopy"]] <- fm3 <- stan_occu(data = unmarkedFrame, formula = ~1 ~Canopy_Height_m)
fm3
#i need to ask elise what the outputs mean

output <- summary(fm3, "state")
logit <- output$mean[2]
odds <- exp(logit)
prob2 <- odds / (1 + odds)
prob2
#psi = 0.586 or ~59% of sites occupied

modlist[["canopy_r"]] <- fm4 <- stan_occu(data = unmarkedFrame, formula = ~1 ~Canopy_Height_m + (1|cams))
fm4
#this ran really, really slow (9 min)
#should we specify chains and iters?
#is there another way to add random effects?

output <- summary(fm4, "state")
logit <- output$mean[2]
odds <- exp(logit)
prob3 <- odds / (1 + odds)
prob3
#psi = 0.637 or ~64% of sites [higher]

#compare
prob2;prob3

#compare models??
