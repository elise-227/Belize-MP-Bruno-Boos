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

sitecov2014 <- read.csv("sitecovs2014.csv")
sitecov2014 <- sitecov2014 %>% rename("Brand" = "Brand.x") #fix errors
sitecov2014 <- sitecov2014 %>% rename("Active_Log" = "Active.Logging")
sitecov2014 <- sitecov2014 %>% rename("Long" = "Lon")
sitecov2014$Month <- str_replace(sitecov2014$Date.Placement.x, pattern = ".*-([^-]*)-.*", replacement = "\\1")
sitecov2014$Season <- ifelse(sitecov2014$Month %in% c("05", "06", "07", "08", "09", "10"), 
                             "Rainy", "Dry")

  sitecov2015 <- read.csv("sitecovs2015.csv")
sitecov2015 <- sitecov2015 %>% rename("Long" = "Lat.1") #fix error
sitecov2015$Month <- str_replace(sitecov2015$Date_Place.x, pattern = ".*-([^-]*)-.*", replacement = "\\1")
sitecov2015$Season <- ifelse(sitecov2015$Month %in% c("05", "06", "07", "08", "09", "10"), 
                             "Rainy", "Dry")

  sitecov2017 <- read.csv("sitecovs2017.csv")
sitecov2017$Month <- str_replace(sitecov2017$Date.Placement, pattern = ".*-([^-]*)-.*", replacement = "\\1")
sitecov2017$Season <- ifelse(sitecov2017$Month %in% c("05", "06", "07", "08", "09", "10"), 
                               "Rainy", "Dry")
  
  sitecov2018 <- read.csv("sitecovs2018.csv")
sitecov2018$Active_Log <- ifelse(sitecov2018$Active_Log == "no", "No", "Yes")
sitecov2018$Month <- str_replace(sitecov2018$Date.Placement, pattern = ".*-([^-]*)-.*", replacement = "\\1")
sitecov2018$Season <- ifelse(sitecov2018$Month %in% c("05", "06", "07", "08", "09", "10"), 
                             "Rainy", "Dry")

  
#add logging column? if ever logged?
sitecov2014$Logged.in.Year <- replace_na(sitecov2014$Logged.in.Year, "No") #na to No
sitecov2014$Year_Since_Logging <- ifelse(sitecov2014$Logged.in.Year == "No", "No","Yes") 
  
#select columns for site covs
sitecov2014 <- select(sitecov2014, c("site", "Lat", "Long", "Brand", "Active_Log", "Month", "Season"))
  sitecov2015 <- select(sitecov2015, c("site", "Lat", "Long", "Brand", "Active_Log", "Month", "Season"))
  sitecov2017 <- select(sitecov2017, c("site", "Lat", "Long", "Brand", "Active_Log", "Month", "Season"))
  sitecov2018 <- select(sitecov2018, c("site", "Lat", "Long", "Brand", "Active_Log", "Month", "Season"))

#bind
sitecov_tojoin <- rbind(sitecov2014, sitecov2015, sitecov2017, sitecov2018)
  
  
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

#now join covs together
covs_2 <- left_join(sitecov_tojoin, covs_2, by = c("site" = "cams"))

covs_2$LatLong <- (covs_2$Lat * covs_2$Long) #new Lat*Long column to account for autocorrelation

#need to join this but not now bc it lowers sites from 256 to 251
#need to add covs: logging, season, month, cam type, time, lat*long, camera
#covs_3 <- left_join(sitecovs, covs_2, by = c("site" = "cams"))


########################################################################
#something wrong#

#remove whats different (IDK which cams it is)
#newdata <- all.equal(sitecovs$site, detect$cams)
#data_merge <- inner_join(sitecovs, detect, by = "site")

##########################################################################

#check all covs - for some reason some have been converted to factors?
#covs_2$cams <- factor(covs_2$cams)
#covs500[,c(28,41,54,67,80,93)] <- lapply(covs500[,c(28,41,54,67,80,93)], as.numeric)


unmarkedFrame <- unmarkedFrameOccu(y = detect_hist_GF$detection_history, siteCovs = covs_2)
summary(unmarkedFrame)


#standardizing variables
unmarkedFrame@siteCovs$Canopy_Height_m <- scale(unmarkedFrame@siteCovs$Canopy_Height_m)
unmarkedFrame@siteCovs$LatLong <- scale(unmarkedFrame@siteCovs$LatLong)

#start modeling
modlist <- list()

f1 <- occu(formula = ~1 # detection formula first
                                      ~1,# occupancy formula second,
                                      data = unmarkedFrame)

backTransform(fm1, type = "state")
backTransform(fm1, type = "det")

###models with Random effect
#ubms is Bayesian
modlist[["int"]] <- f2 <- stan_occu(data = unmarkedFrame, formula = ~1 ~1, chains = 3, iter = 500)
f2 #prob/credible intervals (does not cross zero - positive effect), n_eff is # of effective obs, Rhat convergence diagnostic tells us fit - should be >1
#should do 1000 - 5000 iter

output <- summary(fm2, "state")
logit <- output$mean[1]
odds <- exp(logit)
prob1 <- odds / (1 + odds)
prob1
#occupancy probability ~ 0.623 or we expect to find gray foxes at ~62% of sites

modlist[["canopy"]] <- f3 <- stan_occu(data = unmarkedFrame, formula = ~1 ~Canopy_Height_m, chains = 3, iter = 500)
fm3
#i need to ask elise what the outputs mean

output <- summary(fm3, "state")
logit <- output$mean[2]
odds <- exp(logit)
prob2 <- odds / (1 + odds)
prob2
#psi = 0.586 or ~59% of sites occupied

modlist[["canopy_r"]] <- f4 <- stan_occu(data = unmarkedFrame, formula = ~1 ~Canopy_Height_m + (1|site), chains = 3, iter = 800)
fm4
#this ran really, really slow (9 min)||faster when specified chains and iter

output <- summary(fm4, "state")
logit <- output$mean[2]
odds <- exp(logit)
prob3 <- odds / (1 + odds)
prob3
#psi = 0.649 or ~65% of sites [higher]

prob2;prob3

modlist[["logging"]] <- f5 <- stan_occu(data = unmarkedFrame, formula = ~1 ~Active_Log, chains = 3, iter = 800)
f5

output <- summary(f5, "state")
logit <- output$mean[2]
odds <- exp(logit)
prob4 <- odds / (1 + odds)
prob4
#psi = 0.699

modlist[["logging_r"]] <- f6 <- stan_occu(data = unmarkedFrame, formula = ~1 ~Active_Log + (1|site), chains = 3, iter = 800)
f6

output <- summary(f6, "state")
logit <- output$mean[2]
odds <- exp(logit)
prob5 <- odds / (1 + odds)
prob5
#psi = 0.768

#predict with formula
#0.0996 + 1.1998*Yeslog + 1.5506*site

exp(0.0996 + 1.1998 + 1.5506) / (1 + exp(0.0996 + 1.1998 + 1.5506))
#psi - 0.945
exp(0.0996 + 1.5506) / (1 + exp(0.0996 + 1.5506))
#psi - 0.839

#how to compare models??

###############################################################################
#models in unmarked without random effects
modlist2 <- list()

modlist[["intercept"]] <- fm1 <- occu(formula = ~1 # detection formula first
                                      ~1,# occupancy formula second,
                                      data = unmarkedFrame)

modlist2[["canopy"]] <- fm2 <- occu(formula = ~1
                                   ~Canopy_Height_m,
                                   data = unmarkedFrame)

#modlist2[["Hour"]] <- fm2 <- occu(formula = ~Hour
#                                   ~1,
#                                   data = unmarkedFrame)

modlist2[["Month"]] <- fm2 <- occu(formula = ~Month
                                  ~1,
                                  data = unmarkedFrame) #significant

modlist2[["Season"]] <- fm3 <- occu(formula = ~1
                                 ~Season,
                                 data = unmarkedFrame)

modlist2[["Logging"]] <- fm4 <- occu(formula = ~1
                                    ~Active_Log,
                                    data = unmarkedFrame)

modlist2[["Season_Log"]] <- fm5 <- occu(~1
                                       ~Season + Active_Log,
                                       unmarkedFrame)

modlist2[["Month_Log"]] <- fm6 <- occu(~1
                                      ~Month + Active_Log,
                                      unmarkedFrame) #keep getting Hessuian is singular error message

aictab(modlist2) #month is best (probably bc of Hessuian error)

##predict
#To get real estimate of occupancy (with 95% CI)

predict(fm4, 
        newdata = data.frame(Active_Log = "Yes"),
        type = "state")
predict(fm4, 
        newdata = data.frame(Active_Log = "No"),
        type = "state")
#not working


#To get real estimate of detection (with 95% CI)
predict(fm4, 
        newdata = data.frame(temp_2015_C = mean(unmarkedFrame@siteCovs$temp_2015_C)),
        type = "det")
#0.08