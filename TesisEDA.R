library(ggplot2)
library(gtools)
library(dplyr)

setwd("/Users/ainefernandez/documents/F1DataScience")

#Data 
laps<-read.csv("TesisDryQuickLaps2.csv")
circuitInfo<-read.csv("TesisCircuitInfo.csv")
stints<-read.csv("TesisStints2.csv")
pitstops<-read.csv("TesisPitstops.csv")
outlaps<-read.csv("TesisOutlaps2.csv")
inlaps<-read.csv("TesisInlaps2.csv")
nlaps<-read.csv("TesisNLaps.csv")
safetycars<-read.csv("TesisSafetyCars.csv")

min(laps$LapTimePerKM)
max(laps$LapTimePerKM)

laptimesperkm<-laps$LapTimePerKM
laptimesMclaren<-laps$LapTimePerKM[laps$GP=="Spain"& laps$Team=="McLaren"&laps$Compound=="MEDIUM"]

hist(laptimesMclaren, breaks=15, freq = FALSE, main = "Histogram with Gamma Distribution", xlab = "Lap Time Per KM", col = "lightblue", border = "black")
fit <- fitdistr(laptimesMclaren, "gamma")
curve(dgamma(x, shape = fit$estimate["shape"], rate = fit$estimate["rate"]), 
      col = "red", lwd = 2, add = TRUE)


hist(laptimesmonza,breaks=20, freq = FALSE, main = "Histogram with Normal Distribution", xlab = "Lap Time Per KM", col = "lightblue", border = "black")

# Fit a normal distribution to the data
fit <- fitdistr(laptimesmonza, "normal")
fit2 <- fitdistr(laptimesmonza, "gamma")
fit3 <- fitdistr(laptimesmonza, "lognormal")
# Add the normal distribution curve
curve(dgamma(x, shape = fit2$estimate["shape"], rate = fit2$estimate["rate"]), 
      col = "purple", lwd = 2, add = TRUE)
curve(dnorm(x, mean = mean(laptimesmonza), sd = sqrt(var(laptimesmonza))), 
      col = "red", lwd = 2, add = TRUE)
curve(dlnorm(x, meanlog = fit3$estimate["meanlog"], sdlog = fit3$estimate["sdlog"]), 
      col = "yellow", lwd = 2, add = TRUE)


# Plot the histogram
hist(laps$LapTimePerKM, freq = FALSE, main = "Histogram with Log-Normal Distribution", xlab = "Lap Time Per KM", col = "lightblue", border = "black")

# Fit a log-normal distribution to the data


# Add the log-normal distribution curve
curve(dlnorm(x, meanlog = fit$estimate["meanlog"], sdlog = fit$estimate["sdlog"]), 
      col = "red", lwd = 2, add = TRUE)



library(PredictionR)
b<-bestfit(laps$LapTimePerKM)

library(EnvStats)
library(fitdistrplus)
library(actuar)
distChoose(laps$LapTimePerKM[laps$GP=="Spain"& laps$Team=="McLaren"])
fitdist(laptimesmonza, "weibull",method="mle",start=list(shape=2,scale=1))
invgauss<-fitdist(laptimesmonza, "invgauss",method="mle",start=list(mean=10,shape=1))
invgauss$aic

hist(pitstops$PitstopT)

unique(laps$Team)
