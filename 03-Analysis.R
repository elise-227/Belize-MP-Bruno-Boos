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

names <- unique(dat$Species)
a <- paste0('"', paste(names, collapse='", "'), '"')
print(a, quote=F)


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
cam_op <- cameraOperation(allcamop, stationCol = "site", setupCol = "Date.Placement", 
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

detect_hist_GF <- detectionHistory(recordTable = dat, species = "Gray Fox", 
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
summary(site_covs)

#make unmarked frame
unmarkedFrame <- unmarkedFrameOccu(y = detect_hist_GF$detection_history, siteCovs = site_covs)
summary(unmarkedFrame)


#standardizing variables
unmarkedFrame@siteCovs$Canopy_Height_m <- scale(unmarkedFrame@siteCovs$Canopy_Height_m)
unmarkedFrame@siteCovs$LatLong <- scale(unmarkedFrame@siteCovs$LatLong)
unmarkedFrame@siteCovs$mean_bioma <- scale(unmarkedFrame@siteCovs$mean_bioma)

#start modeling
modlist <- list()
species <- "Gray Fox"
modlist[[paste0(species,"null")]] <-stan_occu(data = unmarkedFrame, formula = ~1 ~1, chains = 4, iter = 5000)

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
