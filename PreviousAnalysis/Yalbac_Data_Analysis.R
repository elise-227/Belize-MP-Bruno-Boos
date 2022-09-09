# Analyze Yalbac camera trap data
# Markus Le
# July 11, 2016

# 1. Initial Setup =====
# install.packages("MuMIn", "RMark", "stringr")
library(RMark)
library(MuMIn)
library(stringr)

spp_name <- "Puma"

SPP <- read.table(paste("chist_", spp_name ,".txt", sep = ""), sep = "\t", colClasses = c(rep("character", 163)), header=TRUE)
WT <- SPP
WT <- WT[, c(8:59)]
for (i in 8:163) {
  SPP[SPP[,i]==".",i] <- "0"
  SPP[,i]=as.numeric(SPP[,i])
} 

SPP <- SPP[,-1]
SPP$Treatment <- as.factor(SPP$Treatment)
SPP[,c(2:5)] <- lapply(SPP[,c(2:5)], as.numeric)
SPP$BA <- scale(SPP$BA)
# SPP$Aspect <- scale(SPP$Aspect)
# SPP$Curvature <- scale(SPP$Curvature)
SPP$Elevation <- scale(SPP$Elevation)
# SPP$Insolation <- scale(SPP$Insolation)
# SPP$Slope <- scale(SPP$Slope)
# SPP$TCI <- scale(SPP$TCI)
# SPP$TPIc <- scale(SPP$TPIc)
SPP$DistRd <- scale(SPP$DistRd)
SPP$DistStrm <- scale(SPP$DistStrm)
# SPP$NDVI <- scale(SPP$NDVI)

# Brand is causing trouble, remove it for now
# SPP[,c(14:117)] <- lapply(SPP[,c(14:117)], as.factor)
# SPP <- SPP[,-c(14:65)]

# Dredge detection variables =====
SPP.models <- mark(SPP, model="Occupancy", model.parameters=list(
  p=list(formula=~Cloud+Temp+Week),
  Psi=list(formula=~BA+Elevation+DistRd+DistStrm+Treatment)),
  groups = c("Treatment"), delete = TRUE, invisible = FALSE)

#calculate chat
mod.chat <- SPP.models$results$deviance/SPP.models$results$deviance.df

#Fix the global Psi model
MuMIn:::fixCoefNames(names(coeffs(SPP.models)))
dd=dredge(SPP.models, fixed=c("Psi(BA)","Psi(Elevation)","Psi(DistRd)","Psi(DistStrm)","Psi(Treatment)"), rank = QAIC, chat = mod.chat) 
#Must fix the Psi parameters to just do p and visa versa for psi dredge
dd
subset(dd, delta <4)

#models with delta.aicc < 4; get model-avg param estimates
p_coefs_subset <-summary(model.avg(dd, subset = delta < 4))
summary(p_coefs_subset)

#full model set; get variable importance ranking
p_coefs <-summary(model.avg(dd))
summary(p_coefs)

dir.create(paste(getwd(), "/pdredge/", sep = ""))
dir.create(paste(getwd(), "/psummary/", sep = ""))
write.csv(dd, paste("pdredge/OccupancyAIC_", spp_name, ".csv", sep = ""))
write.csv(p_coefs_subset$coefmat.full, paste("psummary/PVarImp_", spp_name, ".csv", sep = ""))

# get significant p variables for psi models
p_coefs_subset_table <- as.data.frame(p_coefs_subset$coefmat.subset)
p_coefs_subset_table <- p_coefs_subset_table[-(grep("Psi", row.names(p_coefs_subset_table))),]
p_coefs_subset_table <- p_coefs_subset_table[-(grep("Intercept", row.names(p_coefs_subset_table))),]
p_coefs_subset_table <- p_coefs_subset_table[p_coefs_subset_table$"Pr(>|z|)" <= 0.05,]
p_coefs_names <- row.names(p_coefs_subset_table)

p_coefs_names_trim <- p_coefs_names
p_coefs_names_trim <- str_replace_all(p_coefs_names_trim, "p\\(", "")
p_coefs_names_trim <- str_replace_all(p_coefs_names_trim, "\\)", "")
p_formula <- paste("~", paste(p_coefs_names_trim, collapse = "+"), sep = "")

# Dredge occupancy variables =====
if (length(p_coefs_names_trim) > 0 ) {
  SPP.models.psi=mark(SPP, model="Occupancy",model.parameters=list(
    p=list(formula=as.formula(p_formula)),
    Psi=list(formula=~BA+Elevation+DistRd+DistStrm+Treatment)),
    groups = c("Treatment"), delete = TRUE, invisible = FALSE)
} else {
  SPP.models.psi=mark(SPP, model="Occupancy",model.parameters=list(
    Psi=list(formula=~BA+Elevation+DistRd+DistStrm+Treatment)),
    groups = c("Treatment"), delete = TRUE, invisible = FALSE)
}

#Fix the global Psi model
MuMIn:::fixCoefNames(names(coeffs(SPP.models.psi)))
psidd=dredge(SPP.models.psi, fixed=c(p_coefs_names),rank=QAIC, chat=mod.chat)

#models with delta.aicc < 4; get model-avg param estimates
final_coefs_subset <-summary(model.avg(psidd, subset = delta < 4, fit = TRUE))
summary(final_coefs_subset)

#full model set; get variable importance ranking
final_coefs <-summary(model.avg(psidd, fit = TRUE))
summary(final_coefs)

# Get final coefficients for final model, rerun model =====
# get significant psi variables for final models
final_coefs_subset_table <- as.data.frame(final_coefs_subset$coefmat.subset)
final_coefs_subset_table <- final_coefs_subset_table[-(grep("p", row.names(final_coefs_subset_table))),]
final_coefs_subset_table <- final_coefs_subset_table[-(grep("Treatment", row.names(final_coefs_subset_table))),]
final_coefs_subset_table <- final_coefs_subset_table[final_coefs_subset_table$"Pr(>|z|)" <= 0.05,]
final_coefs_names <- row.names(final_coefs_subset_table)

final_coefs_names_trim <- final_coefs_names
final_coefs_names_trim <- str_replace_all(final_coefs_names_trim, "Psi\\(", "")
final_coefs_names_trim <- str_replace_all(final_coefs_names_trim, "\\)", "")
final_coefs_names_trim <- append(final_coefs_names_trim, "Treatment")
psi_formula <- paste("~", paste(final_coefs_names_trim, collapse = "+"), sep = "")

#Fix the global Psi model
if (length(p_coefs_names_trim) > 0 ) {
  SPP.models.psi2=mark(SPP, model="Occupancy",model.parameters=list(
    p=list(formula=as.formula(p_formula)),
    Psi=list(formula=as.formula(psi_formula))),
    groups = c("Treatment"), delete = TRUE, invisible = FALSE)
} else {
  SPP.models.psi2=mark(SPP, model="Occupancy",model.parameters=list(
    Psi=list(formula=as.formula(psi_formula))),
    groups = c("Treatment"), delete = TRUE, invisible = FALSE)
}

MuMIn:::fixCoefNames(names(coeffs(SPP.models.psi2)))
psidd2=dredge(SPP.models.psi2, fixed=c(p_coefs_names),rank=QAIC, chat=mod.chat)

# Get final coefficients for final model, rerun model =====
#models with delta.aicc < 4; get model-avg param estimates
coefs_subset_psi <-summary(model.avg(psidd2, subset = delta < 4))
summary(coefs_subset_psi)

#full model set; get variable importance ranking
coefs_psi <-summary(model.avg(psidd2))
summary(coefs_psi)

#write results
dir.create(paste(getwd(), "/psidredge/", sep = ""))
dir.create(paste(getwd(), "/psisummary/", sep = ""))
write.csv(psidd2, paste("psidredge/PsiFullQAIC_", spp_name, ".csv", sep = ""))
subset<-subset(psidd2, delta < 4)
write.csv(subset,paste("psidredge/PsiSubsetQAIC_", spp_name, ".csv", sep = ""))

#models with delta.aicc < 4; get model-avg param estimates
coefs_subset_psi <-summary(model.avg(psidd2, subset = delta < 4, fit = TRUE))
psisummary2<-summary(coefs_subset_psi)
psisummary2
write.csv(psisummary2$coefmat.full, paste("psisummary/PsiSummary_", spp_name, ".csv", sep = ""))

#full model set; get variable importance ranking
coefs_psi <-summary(model.avg(psidd2))
psivarimp2<-summary(coefs_psi)
psivarimp2
write.csv(psivarimp2$importance, paste("psisummary/PsiVarImp_", spp_name, ".csv", sep = ""))

# get estimates for p and Psi =====
ssoccmodels <- function() {
  SPPgo.proc <- process.data(SPP, model="Occupancy", group=c("Treatment"))
  SPPgo.ddl <- make.design.data(SPPgo.proc)

  # # Make variable combinations
  # Psi.vars <- c(combn(c("BA", "Elevation", "DistRd", "DistStrm", "Treatment"), 1, simplify = FALSE),
  #               combn(c("BA", "Elevation", "DistRd", "DistStrm", "Treatment"), 2, simplify = FALSE),
  #               combn(c("BA", "Elevation", "DistRd", "DistStrm", "Treatment"), 3, simplify = FALSE),
  #               combn(c("BA", "Elevation", "DistRd", "DistStrm", "Treatment"), 4, simplify = FALSE),
  #               combn(c("BA", "Elevation", "DistRd", "DistStrm", "Treatment"), 5, simplify = FALSE))
  # p.vars <- c(combn(c("Cloud", "Temp", "Week"), 1, simplify = FALSE),
  #             combn(c("Cloud", "Temp", "Week"), 2, simplify = FALSE),
  #             combn(c("Cloud", "Temp", "Week"), 3, simplify = FALSE))

  # Specify parameters
  Psi.vars <- list(formula=as.formula(psi_formula))
  p.dot <- list(formula=~1)

  # Create model list
  SPPmodels <- create.model.list("Occupancy")

  # Send models to MARK for fitting and retrieve results
  SPPgo.results <- mark.wrapper(SPPmodels,
                                data=SPPgo.proc, ddl=SPPgo.ddl, se=FALSE, brief=TRUE, chat = mod.chat)

  # Return results object for assignment
  return(SPPgo.results)
}

# Run function and store results
SPPgo <- ssoccmodels()

# Show -2logLikelihood in model selection table
SPPgo$model.table <- model.table(SPPgo, use.lnl=TRUE)

SPPgo  # Print model selection table

# Model-averaging the real estimates
mavgests.Psi <- model.average(SPPgo, "Psi", vcv=T)$estimates
mavgests.Psi

mavgests.p <- model.average(SPPgo, "p", vcv=T)$estimates
mavgests.p

day.wt <- c()
for (i in 1:52) {
  n.survey <- length(WT[WT!="."])
  day <- WT[,i]
  wt <- length(day[day!="."])/n.survey
  day.wt <- append(day.wt, wt)
}

mavgests.p$Treatment <- as.character(mavgests.p$Treatment)
mavgests.p.mean <- as.data.frame(c())
for (i in unique(mavgests.p$Treatment)) {
  dat <- mavgests.p[mavgests.p$Treatment == i, ]
  avg <- weighted.mean(dat$estimate, day.wt)
  se  <- sqrt(weighted.mean(dat$se^2 + dat$estimate^2, day.wt) - weighted.mean(dat$estimate, day.wt)^2)
  lcl <- avg - se
  ucl <- avg + se
  mavgests.p.mean <- rbind(mavgests.p.mean, cbind(avg, se, lcl, ucl, i))
}
names(mavgests.p.mean) <- c("estimate", "se", "lcl", "ucl", "Treatment")

write.csv(mavgests.Psi, paste("psisummary/PsiEst_", spp_name, ".csv", sep = ""), row.names = FALSE)
write.csv(mavgests.p.mean, paste("psisummary/pEst_", spp_name, ".csv", sep = ""), row.names = FALSE)

# Alternate dredging method =====
# ssoccmodels <- function() {
#   SPPgo.proc <- process.data(SPP, model="Occupancy", group=c("Treatment"))
#   SPPgo.ddl <- make.design.data(SPPgo.proc)
#   
#   # Make variable combinations
#   Psi.vars <- c(combn(c("BA", "Elevation", "DistRd", "DistStrm", "Treatment"), 1, simplify = FALSE), 
#                 combn(c("BA", "Elevation", "DistRd", "DistStrm", "Treatment"), 2, simplify = FALSE), 
#                 combn(c("BA", "Elevation", "DistRd", "DistStrm", "Treatment"), 3, simplify = FALSE), 
#                 combn(c("BA", "Elevation", "DistRd", "DistStrm", "Treatment"), 4, simplify = FALSE), 
#                 combn(c("BA", "Elevation", "DistRd", "DistStrm", "Treatment"), 5, simplify = FALSE))
#   p.vars <- c(combn(c("Cloud", "Temp", "Week"), 1, simplify = FALSE), 
#               combn(c("Cloud", "Temp", "Week"), 2, simplify = FALSE), 
#               combn(c("Cloud", "Temp", "Week"), 3, simplify = FALSE))
#   
#   # Specify parameters
#   Psi.dot <- list(formula=~1)
#   for (i in 1:length(Psi.vars)) {
#     vars <- Psi.vars[[i]]
#     form <- paste("~", paste(vars, collapse = "+"), sep = "")
#     name <- paste("Psi.", paste(vars, collapse = "."), sep = "")
#     assign(name, list(formula=as.formula(form)))
#   }
#   p.dot <- list(formula=~1)
#   for (i in 1:length(p.vars)) {
#     vars <- p.vars[[i]]
#     form <- paste("~", paste(vars, collapse = "+"), sep = "")
#     name <- paste("p.", paste(vars, collapse = "."), sep = "")
#     assign(name, list(formula=as.formula(form)))
#   }
#   
#   # Create model list
#   SPPmodels <- create.model.list("Occupancy")
#   
#   # Send models to MARK for fitting and retrieve results
#   SPPgo.results <- mark.wrapper(SPPmodels,
#                                 data=SPPgo.proc, ddl=SPPgo.ddl, se=FALSE, brief=TRUE, chat = mod.chat)
#   
#   # Return results object for assignment
#   return(SPPgo.results)
# }
# 
# # Run function and store results
# SPPgo <- ssoccmodels()
# 
# # Show -2logLikelihood in model selection table
# SPPgo$model.table <- model.table(SPPgo, use.lnl=TRUE)
# 
# SPPgo  # Print model selection table