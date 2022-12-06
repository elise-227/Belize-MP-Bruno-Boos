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

#fix logging column
covs_all$Logging <- ifelse(covs_all$Logging == c("No", "No Logging"), "No", "Yes")

#look at biomass vs logging
plot <- ggplot(covs_all, aes(x = Logging, y = mean_bioma, color = Logging)) +
  geom_line() +
  geom_point()

#look at logging & FL columns
plot2 <- ggplot(covs_all, aes(x = Logging, y = Forest_Los, color = Logging)) +
  geom_line() +
  geom_point()

xtabs(~Logging + FL, data = covs_all) #noteworthy - we have much more logging sites than no logging sites
#92 sites no logging, 318 sites yes logging
apply(table(covs_all[, c("Logging", "FL")]), 1, prop.table)

#make camera_operation frame
cam_op <- cameraOperation(camop, stationCol = "site", setupCol = "Date.Placement", 
                             retrievalCol = "Last.record", occasionStartTime = 0 , 
                             dateFormat = "%Y-%m-%d", writecsv = FALSE)


#detection history for gray fox

#getting an error for date_time_obs so checking it
IsDate <- function(mydate, date.format = "%Y-%m-%d %H.%M") {
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}

datecheck <- data.frame(IsDate(dat$date_time_obs))
#issues with row 1557, 4439, 4743, 6066, 13493, 15059
dat <- dat[-c(1557,4439,4743,6066,13493,15059),]

species <- unique(dat$Species) #Ocellated Turkey, Agouti, Great Curassow

spe_table <- table(dat$Species)

detect_hist_OT <- detectionHistory(recordTable = dat, species = "Ocellated Turkey", 
                                             camOp = cam_op, output = "binary", stationCol = "site", 
                                             speciesCol = "Species", day1 ="station", 
                                             recordDateTimeCol = "date_time_obs", 
                                             recordDateTimeFormat =  "%Y-%m-%d %H.%M", 
                                             timeZone = "UTC", occasionLength = 1, 
                                             includeEffort = TRUE, scaleEffort = FALSE, 
                                             writecsv = FALSE)

#Agouti
detect_hist_A <- detectionHistory(recordTable = dat, species = "Agouti", 
                                   camOp = cam_op, output = "binary", stationCol = "site", 
                                   speciesCol = "Species", day1 ="station", 
                                   recordDateTimeCol = "date_time_obs", 
                                   recordDateTimeFormat =  "%Y-%m-%d %H.%M", 
                                   timeZone = "UTC", occasionLength = 1, 
                                   includeEffort = TRUE, scaleEffort = FALSE, 
                                   writecsv = FALSE)

#Great Curassow
detect_hist_GC <- detectionHistory(recordTable = dat, species = "Great Curassow", 
                                  camOp = cam_op, output = "binary", stationCol = "site", 
                                  speciesCol = "Species", day1 ="station", 
                                  recordDateTimeCol = "date_time_obs", 
                                  recordDateTimeFormat =  "%Y-%m-%d %H.%M", 
                                  timeZone = "UTC", occasionLength = 1, 
                                  includeEffort = TRUE, scaleEffort = FALSE, 
                                  writecsv = FALSE)


#fix site covs
#site_covs <- covs_all[!duplicated(covs_all$site), ]

#need to join detection history frame with site covs to get cam IDs right
#detect <- as.data.frame(detect_hist_GF$detection_history, row.names = NULL,
#                        stringsAsFactors = FALSE)

#detect$cams <- row.names(detect) #column of cams

#site_covs <- left_join(detect, site_covs, by = c("cams" = "site")) #in this case it was already correct
#site_covs2 <- select(site_covs, c(145:157,256)) #careful here, this will change
##review this? (Note to Elise - did not lose any cameras, just cut out unneeded variables)

######################################################################
#time to model
#check all covs to make sure their class is correct, characters will be converted to factors when making unmarked frame
summary(covs_all)

#make unmarked frame, Ocellated Turkey
unmarkedFrameOT <- unmarkedFrameOccu(y = detect_hist_OT$detection_history, siteCovs = covs_all)
summary(unmarkedFrameOT) ##important, check number of sites with obs

#Agoti
unmarkedFrameA <- unmarkedFrameOccu(y = detect_hist_A$detection_history, siteCovs = covs_all)
summary(unmarkedFrameA)

#Great Curassow
unmarkedFrameGC <- unmarkedFrameOccu(y = detect_hist_GC$detection_history, siteCovs = covs_all)
summary(unmarkedFrameGC)

#standardizing variables
unmarkedFrameOT@siteCovs$Canopy_Height_m <- scale(unmarkedFrameOT@siteCovs$Canopy_Height_m)
unmarkedFrameOT@siteCovs$mean_bioma <- scale(unmarkedFrameOT@siteCovs$mean_bioma)
unmarkedFrameOT@siteCovs$mean_NDVI <- scale(unmarkedFrameOT@siteCovs$mean_NDVI)

#Agoti
unmarkedFrameA@siteCovs$Canopy_Height_m <- scale(unmarkedFrameA@siteCovs$Canopy_Height_m)
unmarkedFrameA@siteCovs$mean_bioma <- scale(unmarkedFrameA@siteCovs$mean_bioma)
unmarkedFrameA@siteCovs$mean_NDVI <- scale(unmarkedFrameA@siteCovs$mean_NDVI)

#GC
unmarkedFrameGC@siteCovs$Canopy_Height_m <- scale(unmarkedFrameGC@siteCovs$Canopy_Height_m)
unmarkedFrameGC@siteCovs$mean_bioma <- scale(unmarkedFrameGC@siteCovs$mean_bioma)
unmarkedFrameGC@siteCovs$mean_NDVI <- scale(unmarkedFrameGC@siteCovs$mean_NDVI)

#start modeling
modlist <- list()


modlist[["null"]] <- f <- occu(data = unmarkedFrameOT, formula = ~1 ~1)

modlist[["noNDVI"]] <- f2 <- occu(data = unmarkedFrameOT, formula = ~1 ~Canopy_Height_m + Logging + mean_bioma + FL)

modlist[["NDVI"]] <- f3 <- occu(data = unmarkedFrameOT, formula =  ~1 ~Canopy_Height_m + Logging + mean_bioma + FL + mean_NDVI)

summary(f)
summary(f2)
summary(f3)

aictab(modlist) #no NDVI is best

#Agoti model
modlist2 <- list()


modlist2[["null"]] <- a <- occu(data = unmarkedFrameA, formula = ~1 ~1)

modlist2[["noNDVI"]] <- a2 <- occu(data = unmarkedFrameA, formula = ~1 ~Canopy_Height_m + Logging + mean_bioma + FL)

modlist2[["NDVI"]] <- a3 <- occu(data = unmarkedFrameA, formula =  ~1 ~Canopy_Height_m + Logging + mean_bioma + FL + mean_NDVI)

summary(a)
summary(a2)
summary(a3)

aictab(modlist2) #NDVI is best

#model
modlist3 <- list()


modlist3[["null"]] <- g <- occu(data = unmarkedFrameGC, formula = ~1 ~1)

modlist3[["noNDVI"]] <- g2 <- occu(data = unmarkedFrameGC, formula = ~1 ~Canopy_Height_m + Logging + mean_bioma + FL)

modlist3[["NDVI"]] <- g3 <- occu(data = unmarkedFrameGC, formula =  ~1 ~Canopy_Height_m + Logging + mean_bioma + FL + mean_NDVI)

modlist3[["log"]] <- g4 <- occu(data = unmarkedFrameGC, formula =  ~1 ~Logging)

modlist3[["FL"]] <- g5 <- occu(data = unmarkedFrameGC, formula =  ~1 ~FL)


summary(g)
summary(g2)
summary(g3)
summary(g4)
summary(g5)

aictab(modlist3) #no NDVI is best (depends on the species) > 1 difference in AIC
#both much better than the null

##mod list for pred
modlist4 <- list()

modlist4[["noNDVI"]] <- g8 <- occu(data = unmarkedFrameGC, formula = ~1 ~Canopy_Height_m + mean_bioma)

modlist4[["noFL/log"]] <- g9 <- occu(data = unmarkedFrameGC, formula =  ~1 ~Canopy_Height_m + mean_bioma + mean_NDVI)

#################################################################
##predict
#To get real estimate of occupancy (with 95% CI)
#fix this - check black bear code
predict(g4, 
        newdata = data.frame(Logging = "Yes"),
        type = "state") # 0.79
predict(g4, 
        newdata = data.frame(Logging = "No"),
        type = "state") #0.81 Higher without logging
#predict FL
predict(g5, 
        newdata = data.frame(FL = "Loss"),
        type = "state") # 0.51
predict(g5, 
        newdata = data.frame(FL = "No Loss"),
        type = "state") #0.82 Much higher

###quick graphs

scaleNDVI <- scale(covs_all$mean_NDVI)

covs_all2 <- covs_all

covs_all2 <- covs_all2[-c(338),]

#join data
indices <- read.csv("indices.csv")

covs_all3 <- left_join(covs_all, indices, by = c("site" = "Name"))

#p1 <- ggplot(data = covs_all2, aes(x = scale(mean_bioma), y = scale(mean_NDVI), color = Logging)) +
#  geom_line() +
#  geom_point()

cbbPalette <- c("#D55E00", "#0072B2")

#save NDVI
#jpeg("NDVI.jpeg")
p <- ggplot(data = covs_all2, aes(x = mean_bioma, y = mean_NDVI, color = Logging)) +
  #  geom_line() +
  geom_point() +
  scale_colour_manual(values = cbbPalette)
#dev.off()

#ARVI
#jpeg("ARVI.jpeg")
p2 <- ggplot(data = covs_all3, aes(x = mean_bioma, y = mean_ARVI, color = Logging)) +
  #  geom_line() +
  geom_point() +
  scale_colour_manual(values = cbbPalette)
#dev.off()

#PRI
#jpeg("PRI.jpeg")
p3 <- ggplot(data = covs_all3, aes(x = mean_bioma, y = mean_PRI, color = Logging)) +
  #  geom_line() +
  geom_point() +
  scale_colour_manual(values = cbbPalette)
#dev.off()

#RGRI
#jpeg("RGRI.jpeg")
p4 <- ggplot(data = covs_all3, aes(x = mean_bioma, y = mean_RGRI, color = Logging)) +
  #  geom_line() +
  geom_point() +
  scale_colour_manual(values = cbbPalette)
#dev.off()

#SIPI
#jpeg("SIPI.jpeg")
p5 <- ggplot(data = covs_all3, aes(x = mean_bioma, y = mean_SIPI, color = Logging)) +
  #  geom_line() +
  geom_point() +
  scale_colour_manual(values = cbbPalette)
#dev.off()

#SR
#jpeg("SR.jpeg")
p6 <- ggplot(data = covs_all3, aes(x = mean_bioma, y = mean_SR, color = Logging)) +
  #  geom_line() +
  geom_point() +
  scale_colour_manual(values = cbbPalette)
#dev.off()

require(ggpubr)
tiff("indices.tiff", width=6.83, height= 5, units="in", res = 300)
ggarrange(p, p2, p3, p4, p5, p6,
                    ncol = 2, nrow = 4)
dev.off()


#################################################################
#t-tests
#var.test(covs_all3$mean_NDVI100, covs_all3$mean_NDVI.x, alternative = "two.sided")
t.test(covs_all3$mean_NDVI.x, covs_all3$mean_NDVI100, paired = TRUE) #fail to reject, not sign. different

t.test(covs_all3$mean_ARVI, covs_all3$mean_ARVI100, paired = T) #fail to rejct

t.test(covs_all3$mean_PRI, covs_all3$mean_PRI100, paired = T) #fail to reject

t.test(covs_all3$mean_RGRI, covs_all3$mean_RGRI100, paired = T) #fail to reject

t.test(covs_all3$mean_SIPI, covs_all3$mean_SIPI100, paired = T) #fail to reject

t.test(covs_all3$mean_SR, covs_all3$mean_SR100, paired = T) #fail to reject


###########################################################
# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
occu_forest_newdata <- data.frame(ndvi = seq(min(covs_all$mean_NDVI), 
                                               max(covs_all$mean_NDVI), by = 0.05),
                                  canopy = mean(covs_all$Canopy_Height_m), # hold other variables constant
                                  bio = mean(covs_all$mean_bioma))

# Model-averaged prediction of occupancy and confidence interval
occu_forest_pred <- modavgPred(modlist4,
                               # c.hat =    # to change variance inflation factor, default = 1) 
                               parm.type = "psi", # psi = occupancy
                               newdata = occu_forest_newdata)[c("mod.avg.pred",
                                                                "lower.CL",
                                                                "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
occu_forest_pred_df <- data.frame(Predicted = occu_forest_pred$mod.avg.pred,
                                  lower = occu_forest_pred$lower.CL,
                                  upper = occu_forest_pred$upper.CL,
                                  occu_forest_newdata)

# Plot the relationship
occu_forest_pred_plot <- ggplot(occu_forest_pred_df, aes(x = forest, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed") +
  geom_path(size = 1) +
  labs(x = "Forest cover (standardized)", y = "Occupancy probability") +
  theme_classic() +
  coord_cartesian(ylim = c(0,1)) +
  theme(text = element_text(family = "HelveticaNeue", colour = "black"),
        axis.text = element_text(colour = "black"))
occu_forest_pred_plot

######################################################################


#To get real estimate of detection (with 95% CI)
predict(fm4, 
        newdata = data.frame(temp_2015_C = mean(unmarkedFrame@siteCovs$temp_2015_C)),
        type = "det")
#0.08

#################################################################################
#run biomass model as a quick test
modlist[["biom"]] <- f <- stan_occu(data = unmarkedFrame, formula = ~1 ~mean_bioma, chains = 4, iter = 10000)
f

output <- summary(f, "state")
logit <- output$mean[2]
boot::inv.logit(logit) #0.433

plot_effects(f, "state")

###models with Random effect
#ubms is Bayesian
modlist[["null"]] <- f1 <- stan_occu(data = unmarkedFrame, formula = ~1 ~1, chains = 4, iter = 10000)
f1
#prob/credible intervals (does not cross zero - positive effect), n_eff is # of effective obs, Rhat convergence diagnostic tells us fit - should be >1
#should do 1000 - 5000 iterations

output <- summary(f1, "state")
logit <- output$mean[1]
odds <- exp(logit)
prob1 <- odds / (1 + odds)
prob1

## jp - if you load the boot package you can use inv.logit() for probability
## jp - what you did is great, just saying...

#occupancy probability ~ 0.668 or we expect to find gray foxes at ~68% of sites

modlist[["canopy"]] <- f2 <- stan_occu(data = unmarkedFrame, formula = ~1 ~Canopy_Height_m, chains = 4, iter = 10000)
f2

output <- summary(f2, "state")
logit <- output$mean[2]
odds <- exp(logit)
prob2 <- odds / (1 + odds)
prob2
#psi = 0.499 or ~50% of sites occupied


modlist[["canopy_r"]] <- f3 <- stan_occu(data = unmarkedFrame, formula = ~1 ~scale(Canopy_Height_m) + 
                                           (1|cams), chains = 4, iter = 10000)
f3

traceplot(f3, pars=c("beta_state", "beta_det"))
 plot_residuals(f3, submodel="state")
  plot_residuals(f3, submodel="state", covariate="Canopy_Height_m")
 
output <- summary(f3, "state")
logit <- output$mean[2]
odds <- exp(logit)
prob3 <- odds / (1 + odds)
prob3
#psi = 0.499 or ~50% of sites [no difference] but failed to converge so increase iterations

boot::inv.logit(logit)


## jp - you can compare models as...
## jp - this suggests the null model is best (yikes) but the last model didn't converge
## no worries bc this doesn't include most of our variables (yet)
mods <- fitList(f1, f2, f3)
round(modSel(mods), 3)


prob2;prob3 #compare without random effect to with random effect

modlist[["logged"]] <- f4 <- stan_occu(data = unmarkedFrame, formula = ~1 ~Logging, chains = 4, iter = 5000)
f4

modlist[["logging_r"]] <- f5 <- stan_occu(data = unmarkedFrame, formula = ~1 ~Logging + (1|cams), chains = 3, iter = 5000)
f5

output <- summary(f4, "state")
logit <- output$mean[2]
odds <- exp(logit)
prob4 <- odds / (1 + odds)
prob4
#psi = 0.589 or 59%

output <- summary(f5, "state")
logit <- output$mean[2]
odds <- exp(logit)
prob5 <- odds / (1 + odds)
prob5
#psi = 0.657 or 66%

#predict with formula Issue here I think
#0.750 + 0.648*LoggingYes + 2.281*site

exp(0.750 + 0.648 + 2.281) / (1 + exp(0.750 + 0.648 + 2.281))
#psi with logging at site - 0.975

exp(0.750 + 2.281) / (1 + exp(0.750 + 2.281))
#psi without logging at site - 0.954
