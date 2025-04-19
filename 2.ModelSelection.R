# 2. MODEL SELECTION
# This R script implements the model selection process described in Chapter 4: 
# "El plan perfecto para la victoria: Modelos".
# It selects the best model for the following dependent variables: LapTimePerKM, PitstopTime, Outlaps, Inlaps, 
# and Safety Car Probability.
# The script uses the bestglm function to perform variable selection within each of three distribution families: 
# Gamma, Log-Normal, and Inverse Gaussian.
# bestglm relies on the Bayesian Information Criterion (BIC) for variable selection.
# To determine the best overall model across the three families, the Akaike Information Criterion (AIC) is used.

library(stargazer)
library(dplyr)
library(car)
library(bestglm)

# Data
data<-read.csv("DryQuickLaps.csv")
circuitInfo<-read.csv("CircuitInfo.csv")
stints<-read.csv("Stints.csv")
nlaps<-read.csv("NLaps.csv")
#Pitstops
datapitstops<-read.csv("PitstopsWithTeams.csv")
#Outlaps
dataoutlaps<-read.csv("Outlaps.csv")
#Inlaps
datainlaps<-read.csv("Inlaps.csv")
#SafetyCars
safetycars<-read.csv("SafetyCars.csv")
safetycars$SafetyCar <- ifelse(safetycars$Label == "Safety Car", 1, 0)

# Convert the appropriate variables to factors
data$Driver <- as.factor(data$Driver)
data$Team <- as.factor(data$Team)
data$Compound <- as.factor(data$Compound)
data$Year <- as.factor(data$Year)
data$GP <- as.factor(data$GP)
dataoutlaps1$Driver<-factor(dataoutlaps1$Driver)
dataoutlaps1$Team<-factor(dataoutlaps1$Team)
dataoutlaps1$Compound<-factor(dataoutlaps1$Compound)
dataoutlaps1$GP<-factor(dataoutlaps1$GP)
datainlaps$Driver<-factor(datainlaps$Driver)
datainlaps$Team<-factor(datainlaps$Team)
datainlaps$Compound<-factor(datainlaps$Compound)
datainlaps$GP<-factor(datainlaps$GP)
# Remove the "X" column if it exists
data <- data[ , !names(data) %in% "X"]

#LAPTIMEPERKM:BESTGLM
data1<-select(data,-LapTimePerKM,-Abrasion,-Traction,-Braking,-TrackEvo,-Grip,-Lateral,-Downforce,-TyreStress,-Length,-Year,-LapTime,-LapNumber,-Laps)
data1
LapTimePerKM<-data$LapTimePerKM
data1<-cbind(data1,LapTimePerKM)
#GAMMA
bestgamma<-bestglm(data1,family=Gamma())
m1gamma<-glm(LapTimePerKM ~GP+RacePercentage+Driver+Team+TyreLife+Compound+Position+Stint, data = data, family = Gamma())
summary(m1gamma)
#NORMAL ln
bestnormal<-bestglm(data1,family = gaussian(link="log"))
m1normal<-glm(LapTimePerKM ~GP+RacePercentage+Driver+Team+TyreLife+Compound+Position+Stint, data = data, family = gaussian(link="log"))
summary(m1normal)
#InvGauss
bestinvGauss<-bestglm(data1,family = inverse.gaussian)
m1invGauss<-glm(LapTimePerKM ~GP+RacePercentage+Driver+Team+TyreLife+Compound+Position+Stint, data = data, family = inverse.gaussian )
summary(m1invGauss)

stargazer(m1invGauss,
          title = "LapTimePerKM",
          header = FALSE,
          model.names = FALSE,
          dep.var.labels.include = TRUE,
          omit.stat = c("LL", "ser", "f"),
          single.row = T)

# PITSTOPS: BESTGLM
data2<-select(datapitstops,-PitstopT,-Unnamed..0,-GP,-Year)
data2
PitstopT<-datapitstops$PitstopT
data2<-cbind(data2,PitstopT)

#GAMMA
bestgammapit<-bestglm(data2,family=Gamma())
pitstopgamma<-glm(PitstopT~Circuit, data=datapitstops,family=Gamma())
summary(pitstopgamma)

#NORMAL ln
bestnormalpit<-bestglm(data2,family=gaussian(link="log"))
pitstopnormal<-glm(PitstopT~Circuit, data=datapitstops,family=gaussian(link="log"))
summary(pitstopnormal)

#InvGauss
bestinvgausspit<-bestglm(data2,family=inverse.gaussian)
pitstopinvgauss<-glm(PitstopT~Circuit, data=datapitstops,family=inverse.gaussian)
summary(pitstopinvgauss)


stargazer(pitstopinvgauss,
          title = "Pitstop Time",
          header = FALSE,
          model.names = FALSE,
          dep.var.labels.include = TRUE,
          omit.stat = c("LL", "ser", "f"),
          single.row = T)

# OUTLAPS: BESTGLM
dataoutlaps1 <- dataoutlaps %>%
  select(-c(Length, Abrasion, Traction, Braking, TrackEvo, Grip, Lateral, Downforce, TyreStress,
            IsAccurate, Unnamed..0,LapNumber, LapTime,Year))

#GAMMA
bestgammaout<-bestglm(dataoutlaps1,family=Gamma())
outgamma<-glm(LapTimePerKM~GP+Compound, data=dataoutlaps,family=Gamma())
summary(outgamma)

#NORMAL ln
bestnormalout<-bestglm(dataoutlaps1,family=gaussian(link="log"))
outnormal<-glm(LapTimePerKM~GP+Compound, data=dataoutlaps,family=gaussian(link="log"))
summary(outnormal)

#InvGauss
bestinvgaussout<-bestglm(dataoutlaps1,family=inverse.gaussian)
outinvgauss<-glm(LapTimePerKM~GP+Compound, data=dataoutlaps,family=inverse.gaussian)
summary(outinvgauss)

stargazer(outinvgauss,
          title = "LapTimePerKM-Outlaps",
          header = FALSE,
          model.names = FALSE,
          dep.var.labels.include = TRUE,
          omit.stat = c("LL", "ser", "f"),
          single.row = T)

# INLAPS: BESTGLM
datainlaps1 <- datainlaps %>%
  select(-c(IsAccurate, Year,Unnamed..0, Length, Abrasion, Traction, Braking, TrackEvo, Grip, Lateral, Downforce, TyreStress,LapNumber, LapTime,Year,LapNumber))
#GAMMA
bestgammain<-bestglm(datainlaps1,family=Gamma())
ingamma<-glm(LapTimePerKM~GP+Compound+TyreLife+Stint, data=datainlaps,family=Gamma())
summary(ingamma)

#NORMAL ln
bestnormalin<-bestglm(datainlaps1,family=gaussian(link="log"))
innormal<-glm(LapTimePerKM~GP+Compound+TyreLife+Stint, data=datainlaps,family=gaussian(link="log"))
summary(innormal)

#InvGauss
bestinvgaussin<-bestglm(datainlaps1,family=inverse.gaussian)
ininvgauss<-glm(LapTimePerKM~GP+Compound+TyreLife+Stint, data=datainlaps,family=inverse.gaussian)
summary(ininvgauss)

stargazer(ininvgauss,
          title = "LapTimePerKM-Inlaps",
          header = FALSE,
          model.names = FALSE,
          dep.var.labels.include = TRUE,
          omit.stat = c("LL", "ser", "f"),
          single.row = T)

# SAFETY CARS: BESTGLM 
safetycars <- safetycars %>%
  select(-c(TrackStatus,Year,Label))
safetycars1$GP<-factor(safetycars1$GP)
bestbinary<-bestglm(safetycars1,family=binomial())

safetycarsmodel<-glm(SafetyCar~GP+LapNumber, data=safetycars,family=binomial())
summary(safetycarsmodel)
safetycarsmodel2<-glm(SafetyCar~LapNumber, data=safetycars,family=binomial())
summary(safetycarsmodel2)


stargazer(safetycarsmodel,
          title = "Probabilidad de Safety Car",
          header = FALSE,
          model.names = FALSE,
          dep.var.labels.include = TRUE,
          omit.stat = c("LL", "ser", "f"),
          digits = 2,
          single.row = T)