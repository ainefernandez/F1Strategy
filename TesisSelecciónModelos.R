library(ggplot2)
library(gtools)
library(stargazer)
library(car)
library(dplyr)
library(bestglm)
View(dta)
#data<-read.csv("TesisDryQuickLaps2.csv")
data<-read.csv("TesisDryQuickLaps3.csv")
China2022 <- data %>%
  filter(GP == "Netherlands", Year == 2022) 
dataWithWet<- read.csv("TesisDataWithWet.csv")
circuitInfo<-read.csv("TesisCircuitInfo.csv")
stints<-read.csv("TesisStints2.csv")
nlaps<-read.csv("TesisNLaps.csv")


data$Year <- as.integer(as.character(data$Year))  # Convert factor to character first, then to integer
wet_laps$Year <- as.integer(wet_laps$Year)  # Convert Year to integer

# Merge the data frames after ensuring Year is of the same type
datawithwet <- data %>%
  left_join(wet_laps, by = c("GP", "Year"))

# Create the WetTrack variable: 1 if Compound is 'WET' or 'INTER', otherwise 0
datawithwet <- datawithwet %>%
  mutate(WetTrack = ifelse(Compound %in% c("WET", "INTER"), 1, 0))

# View the updated DataFrame
print(datawithwet)
names(data)
str(data)
# Convert the appropriate variables to factors
data$Driver <- as.factor(data$Driver)
data$Team <- as.factor(data$Team)
data$Compound <- as.factor(data$Compound)
data$Year <- as.factor(data$Year)
data$GP <- as.factor(data$GP)
data$Abrasion <- as.factor(data$Abrasion)
data$Traction <- as.factor(data$Traction)
data$Braking <- as.factor(data$Braking)
data$TrackEvo <- as.factor(data$TrackEvo)
data$Grip <- as.factor(data$Grip)
data$Lateral <- as.factor(data$Lateral)
data$Downforce <- as.factor(data$Downforce)
data$TyreStress <- as.factor(data$TyreStress)

# Remove the "X" column if it exists
data <- data[ , !names(data) %in% "X"]

data



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





#Pitstops
datapitstops<-read.csv("TesisPitstopsWithTeams.csv")
datapitstops$Driver<-factor(datapitstops$Driver)
datapitstops$Team<-factor(datapitstops$Team)
datapitstops$Circuit<-factor(datapitstops$Circuit)
datapitstops$PitstopT<-as.numeric(datapitstops$PitstopT)

View(datapitstops)

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


#Outlaps
dataoutlaps<-read.csv("TesisOutlaps3.csv")
dataoutlaps1 <- dataoutlaps %>%
  select(-c(Length, Abrasion, Traction, Braking, TrackEvo, Grip, Lateral, Downforce, TyreStress,
            IsAccurate, Unnamed..0,LapNumber, LapTime,Year))
dataoutlaps1$Driver<-factor(dataoutlaps1$Driver)
dataoutlaps1$Team<-factor(dataoutlaps1$Team)
dataoutlaps1$Compound<-factor(dataoutlaps1$Compound)
dataoutlaps1$GP<-factor(dataoutlaps1$GP)

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


#Inlaps
datainlaps<-read.csv("TesisInlaps3.csv")
datainlaps$Driver<-factor(datainlaps$Driver)
datainlaps$Team<-factor(datainlaps$Team)
datainlaps$Compound<-factor(datainlaps$Compound)
datainlaps$GP<-factor(datainlaps$GP)
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

#lluvia
dataWithWet<- read.csv("TesisDataWithWet.csv")
dataWithWet1 <- dataWithWet %>%
  select(-c(X,Compound,LapTimePerKM,LapTime,Year,Laps, Length, Abrasion, Traction, Braking, TrackEvo, Grip, Lateral, Downforce, TyreStress,LapNumber, LapTime,Year))
dataWithWet1$Driver<-factor(dataWithWet1$Driver)
dataWithWet1$Team<-factor(dataWithWet1$Team)
dataWithWet1$GP<-factor(dataWithWet1$GP)
bestlluvia<-bestglm(dataWithWet1,family=binomial())


lluvia<-glm(WetTrack~RacePercentage+GP+Team,data=dataWithWet,family=binomial())
summary(lluvia)
lluvia2<-glm(WetTrack~RacePercentage+GP,data=dataWithWet,family=binomial())
summary(lluvia2)

#SafetyCars
safetycars<-read.csv("TesisSafetyCars2.csv")
levels(safetycars$GP)
safetycars$SafetyCar <- ifelse(safetycars$Label == "Safety Car", 1, 0)
safetycars1 <- safetycars %>%
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





View(safetycars)
SafetyCarByCircuit <- safetycars %>%
  group_by(GP, Year) %>%  # Group by Circuit and Year
  summarise(SafetyCarOccurred = ifelse(sum(SafetyCar) > 0, 1, 0)) %>%  # Mark 1 if any SafetyCar occurred
  group_by(GP) %>%        # Group by Circuit
  summarise(
    TotalSafetyCarOccurrences = sum(SafetyCarOccurred),  # Sum of Safety Car occurrences
    TotalRaces = n()                                     # Total races (years) per circuit
  ) %>%
  mutate(OccurrencePercentage = (TotalSafetyCarOccurrences / TotalRaces) * 100)  # Calculate percentage

# View the result

View(SafetyCarByCircuit)

SafetyCarByCircuit <- SafetyCarByCircuit %>%
  mutate(RiskCategory = case_when(
    OccurrencePercentage <= 30 ~ "Low Risk",
    OccurrencePercentage > 30 & OccurrencePercentage <= 70 ~ "Medium Risk",
    OccurrencePercentage > 70 ~ "High Risk"
  ))

# View the result
print(SafetyCarByCircuit)
