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
#covs_3 <- left_join(sitecovs, covs_2, by = c("site" = "cams"))


########################################################################
#something wrong#

#remove whats different (IDK which cams it is)
newdata <- all.equal(sitecovs$site, detect$cams)
data_merge <- inner_join(sitecovs, detect, by = "site")

##########################################################################


unmarkedFrame <- unmarkedFrameOccu(y = detect_hist_GF$detection_history, siteCovs = covs_2)
#same number of rows but clearly there is some issue with the camera IDs differences?????
summary(unmarkedFrame)




################################################################################
fm <- occu(formula = ~1
           ~1,
           data = unmarkedFrame)
summary(fm)

backTransform(fm, type = "state")
backTransform(fm, type = "det")

###Random effect

model <- stan_occu(data = unmarkedFrame, formula = ~1 ~1)
model

output <- summary(model, "state")
logit<-output$mean[1]
odds <- exp(logit)
prob <- odds / (1 + odds)
prob

model_log <- stan_occu(data = unmarkedFrame, formula = ~1 ~Logging)
model_log
output <- summary(model_log, "state")
logit <- output$mean[2]
odds <- exp(logit)
prob2 <- odds / (1 + odds)
prob2

model_rand <- stan_occu(data = unmarkedFrame, formula = ~1 ~Logging + (1|site))
model_rand

output <- summary(model_rand, "state")
logit <- output$mean[2]
odds <- exp(logit)
prob <- odds / (1 + odds)
prob

prob;prob2
