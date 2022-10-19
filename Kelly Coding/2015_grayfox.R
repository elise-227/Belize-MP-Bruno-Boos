###2015
setwd("~/Belize-MP-Bruno-Boos")

#clear environment
rm(list=ls())
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

#load data
data_2015 <- read.csv("data2015_final.csv")
camop_2015 <- read.csv("camop_2015.csv")

data_2015_fox <- filter(data_2015, Species == "Gray Fox")

#detection history creation
require(camtrapR)
#load in data
camop2015 <- read.csv("camop_2015.csv", stringsAsFactors = TRUE)
data2015 <- read.csv("data2015_final.csv", stringsAsFactors =  TRUE)
covariates <- read_xlsx("Covariates_cams.xlsx")

#camera op frame
camop2015 <- cameraOperation(camop2015, stationCol = "site", setupCol = "Date_Place", retrievalCol = "Last.Record", occasionStartTime = 0 , dateFormat = "%Y-%m-%d", writecsv = FALSE)

#detectopm history matrix
detect_hist_2015_grayfox <- detectionHistory(recordTable = data2015, species = "Gray Fox", camOp= camop2015, output = "binary", stationCol = "site", speciesCol = "Species", day1 ="station", recordDateTimeCol = "date_time_obs", recordDateTimeFormat =  "%Y-%m-%d %H.%M", timeZone = "UTC", occasionLength = 1, includeEffort = TRUE, scaleEffort = FALSE, writecsv = FALSE)

#detectionhist <- detect_hist_2015_grayfox$detection_history

##prepare unmarked frame##

#covariates for only 2015 cams
covs500 <- covariates[76:140,]

#create df of detection to double check covariate cams
dataframe_det <- as.data.frame(detect_hist_2015_grayfox)
dataframe_det$rn <- row.names(dataframe_det)

#join and clean covariate frame
join <- left_join(dataframe_det,covs500, by = c("rn" = "CameraID"))
covs500 <- select(join, 185:284)

#check all covs - for some reason some have been converted to factors?
covs500$rn <- factor(covs500$rn)
covs500[,c(28,41,54,67,80,93)] <- lapply(covs500[,c(28,41,54,67,80,93)], as.numeric)

#make unmarked frame
unmarkedFrame <- unmarkedFrameOccu(detect_hist_2015_grayfox$detection_history, siteCovs = covs500)

#standardizing variables - need to do others but this is a test
unmarkedFrame@siteCovs$Avg_pr_2015 <- scale(unmarkedFrame@siteCovs$Avg_pr_2015)
unmarkedFrame@siteCovs$Canopy_Height_m <- scale(unmarkedFrame@siteCovs$Canopy_Height_m)
unmarkedFrame@siteCovs$temp_2015_C <- scale(unmarkedFrame@siteCovs$temp_2015_C)

summary(unmarkedFrame)

##build models and compare
##added .V1 to the variables for some reason? i.e. Canopy_Height_m.V1

modlist <- list()

modlist[["intercept"]] <- fm1 <- occu(formula = ~1 # detection formula first
                                      ~1,# occupancy formula second,
                                      data = unmarkedFrame)

modlist[["precip"]] <- fm2 <- occu(formula = ~1
                                   ~Avg_pr_2015,
                                   data = unmarkedFrame)

modlist[["canopy"]] <- fm3 <- occu(formula = ~1
                                   ~Canopy_Height_m,
                                   data = unmarkedFrame)

modlist[["temp"]] <- fm4 <- occu(formula = ~1
                                 ~temp_2015_C,
                                 data = unmarkedFrame)

#modlist[["pre_can"]] <- fm5 <- occu(formula = ~1
#                                    ~Avg_pr_2015 + Canopy_Height_m,
#                                    data = unmarkedFrame)

##add one with both covs
fmlist <- fitList(fits = modlist)

modSel(fmlist)

aictab(modlist)
#both imply temperature model is best
#p value of temp is 0.09 so not great


##predict
#To get real estimate of occupancy (with 95% CI)

predict(fm4, 
        newdata = data.frame(temp_2015_C = mean(unmarkedFrame@siteCovs$temp_2015_C)),#idk if it makes sense to set this to 1?
        type = "state")
#0.83 predicted occupancy

#To get real estimate of detection (with 95% CI)
predict(fm4, 
        newdata = data.frame(temp_2015_C = mean(unmarkedFrame@siteCovs$temp_2015_C)),
        type = "det")
#0.08

##plotting relationships

# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
occu_temp_newdata <- data.frame(temp_2015_C = seq(min(unmarkedFrame@siteCovs$temp_2015_C), 
                                               max(unmarkedFrame@siteCovs$temp_2015_C), by = 0.5),
                                Avg_pr_2015 = mean(unmarkedFrame@siteCovs$Avg_pr_2015))#, # hold other variables constant
                                #Canopy_Height_m = mean(unmarkedFrame@siteCovs$Canopy_Height_m)) # hold other variables constant


# Model-averaged prediction of occupancy and confidence interval
occu_temp_pred <- modavgPred(modlist,
                               # c.hat =    # to change variance inflation factor, default = 1) 
                               parm.type = "psi", # psi = occupancy
                               newdata = occu_temp_newdata)[c("mod.avg.pred",
                                                                "lower.CL",
                                                                "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
occu_temp_pred_df <- data.frame(Predicted = occu_temp_pred$mod.avg.pred,
                                  lower = occu_temp_pred$lower.CL,
                                  upper = occu_temp_pred$upper.CL,
                                occu_temp_newdata)

# Plot the relationship
occu_temp_pred_plot <- ggplot(occu_temp_pred_df, aes(x = temp_2015_C, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed") +
  geom_path(size = 1) +
  labs(x = "Average Temperature 2015 (standardized)", y = "Occupancy probability") + #my temp might be the wrong one (not standardized)
  theme_classic() +
  coord_cartesian(ylim = c(0,1)) +
  theme(text = element_text(family = "HelveticaNeue", colour = "black"),
        axis.text = element_text(colour = "black"))
occu_temp_pred_plot
#doesn't look quiet right so figure that out
#should have a CI around it
#NaNs were produced
#will try removing the intercept model
#that didnt work?
#fm3 is the issue
#now it works