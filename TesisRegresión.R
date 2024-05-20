library(ggplot2)
library(gtools)
data<-read.csv("TesisDryQuickLaps.csv")
circuitInfo<-read.csv("TesisCircuitInfo.csv")
stints<-read.csv("TesisStints.csv")
data$Team<-as.factor(data$Team)
data$Compound<-as.factor(data$Compound)
data$Year<-as.factor(data$Year)
data<-within(data,Team<-relevel(Team,ref="Red Bull Racing"))
data<-within(data,Compound<-relevel(Compound,ref="SOFT"))
data<-within(data,Year<-relevel(Year,ref="2023"))
model<-lm(LapTimePerKM~TyreLife+RacePercentage+factor(Compound)+factor(Team)+factor(GP)+factor(Stint)
            ,data=data)
summary(model)

class(data$RacePercentage)
TyreLife<-15
RacePercentage<-2/10
Compound<-"SOFT"
Team<-"Aston Martin"
GP<-"Mexico"
Stint<-1
newdata<-as.data.frame(cbind(Tyrelife,as.numeric(RacePercentage),Compound,Team,GP,Stint))
predict(model,newdata=newdata,type="response")


model2<-lm(LapTimePerKM~TyreLife+factor(Compound)+factor(Team)+factor(Year)+factor(GP),data=data)
summary(model2)


dataplot<-subset(data,GP=="Mexico" & Year==2022 & Team=="Red Bull Racing")

ggplot(dataplot,aes(TyreLife,LapTimePerKM))+
  geom_point()+
  geom_smooth(method = "lm")

dataplot2<-dataplot[order(dataplot$Driver,dataplot$LapNumber),]
stintsMex<-subset(stints, GP=="Mexico")

mean(stintsMex$StintLength[stintsMex$Compound=="SOFT"])
max(stintsMex$StintLength[stintsMex$Compound=="SOFT"])
sd(stintsMex$StintLength[stintsMex$Compound=="SOFT"])
TyreLife<-36
Compound<-"SOFT"
Team<-"Aston Martin"
Year<-2023
newdata<-cbind(TyreLife,Compound,Team,circuitInfo[19,3:11],Year)
predict(model,newdata=newdata,type="response")*circuitInfo$Length[19]

model2<-lm(LapTimePerKM~TyreLife+factor(Compound)+factor(Team)+factor(Abrasion)+
            factor(Traction)+factor(Braking)+factor(TrackEvo)+factor(Grip)+
            factor(Lateral)+factor(Downforce),data=data)

summary(model2)
datapitstops<-read.csv("TesisPitstops.csv")
modelpit<-lm(as.numeric(PitstopT)~factor(Circuit),data=datapitstops)
summary(modelpit)

dataoutlaps<-read.csv("TesisOutlaps.csv")
modelout<-lm(LapTimePerKM~factor(GP)+factor(Compound),data=dataoutlaps)
summary(modelout)

datainlaps<-read.csv("TesisInlaps.csv")
modelin<-lm(LapTimePerKM~factor(GP),data=datainlaps)
summary(modelin)

newdatain<-as.data.frame("Mexico")
predict(modelin,newdata=newdatain,type="response")

nlaps<-read.csv("TesisNLaps.csv")


vidapromedio<-function(Circuito,stints){
  datastints<-subset(stints,GP==Circuito)
  MS<-mean(datastints$StintLength[datastints$Compound=="SOFT"])
  MaxS<-max(datastints$StintLength[datastints$Compound=="SOFT"])
  SdS<-sd(datastints$StintLength[datastints$Compound=="SOFT"])
  MM<-mean(datastints$StintLength[datastints$Compound=="MEDIUM"])
  MaxM<-max(datastints$StintLength[datastints$Compound=="MEDIUM"])
  SdM<-sd(datastints$StintLength[datastints$Compound=="MEDIUM"])
  MH<-mean(datastints$StintLength[datastints$Compound=="HARD"])
  MaxH<-max(datastints$StintLength[datastints$Compound=="HARD"])
  SdH<-sd(datastints$StintLength[datastints$Compound=="HARD"])
  res<-as.data.frame(cbind(MS,MaxS,SdS,MM,MaxM,SdM,MH,MaxH,SdH))
  return(res)
}





tiempoStint <- function(Circuito, Compound, Equipo, Stint, nlaps, circuitInfo, modelo, stints,top,low) {
  dataS <- vidapromedio(Circuito, stints)
  lapsTot <- nlaps$Laps[nlaps$GP == Circuito]
  kmperlap <- circuitInfo$Length[circuitInfo$GP == Circuito]
  TStint <- 0
  #if (Compound == "HARD") {
    #top <- floor(dataS$MH - 1)
  #} #else if (Compound == "MEDIUM") {
    #top <- floor(dataS$MM - 1)
  #} #else {
    #top <- floor(dataS$MS - 1)
  #}
  for (i in low:top) {
    newdata <- data.frame(TyreLife = i,
                          RacePercentage = i / lapsTot,
                          Compound = Compound,
                          Team = Equipo,
                          GP = Circuito,
                          Stint = Stint)
    TStint <- TStint + predict(model, newdata = newdata, type = "response") * kmperlap
    
  }
  
  return(TStint)
}




  
pitstopcost<-function(modelin,modelpit,modelout,Circuito,Compound,circuitInfo){
  kmperlap <- circuitInfo$Length[circuitInfo$GP == Circuito]
  newdatain<-data.frame(GP=Circuito)
  newdatapit<-data.frame(Circuit=Circuito)
  newdataout<-data.frame(GP=Circuito,
                         Compound=Compound)
  inlap<-predict(modelin,newdata=newdatain,type="response")*kmperlap
  outlap<-predict(modelout,newdata=newdataout,type="response")*kmperlap
  pit<-predict(modelpit,newdata=newdatapit,type="response")
  TPit<-inlap+pit+outlap
  return(TPit)
}



modeloDeterminista<-function(Circuito,Equipo,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout){
  dataS<-vidapromedio(Circuito, stints)
  lapsTot <- nlaps$Laps[nlaps$GP == Circuito]
  #1er Stint
  tH<-tiempoStint(Circuito,"HARD",Equipo,1,nlaps,circuitInfo,modelo,stints,floor(dataS$MH-1),1)
  tM<-tiempoStint(Circuito,"MEDIUM",Equipo,1,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1),1)
  tS<-tiempoStint(Circuito,"SOFT",Equipo,1,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1),1)
  
  #RAMA HARDS1erStint
  tHH<-tH+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)
  if (2*floor(dataS$MH)>lapsTot){
    tHH<-tHH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH-2),2)
    tHHM<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)
    tHHS<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)
  }else{
    tHH<-tHH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MH-1),2)
    tHHM<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MH),2)
    tHHS<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MH),2)
  }
  
  tHM<-tH+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)
  #no parar
  if ((lapsTot-floor(dataS$MH))>dataS$MM){
    w1<-paste0("Warning(tHM1): NLaps exceeds expected TyreLife by ", lapsTot - floor(dataS$MH) - dataS$MM," Laps possible with maxTyreLife ", dataS$MaxM-dataS$MM)
  }
  tHM1<-tHM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH),2)
  #Si parar
  tHM2<-tHM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1),2)
  
  #3er stint Softs (mediums 2ndo stint,hards 1er stint,Si parar)
  tHM2S<-tHM2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2)
  #3er stint Mediums (mediums 2ndo stint,hards 1er stint,Si parar)
  tHM2M<-tHM2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2)
  #3er stint Hards (mediums 2ndo stint,hards 1er stint,Si parar)
  tHM2H<-tHM2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2)
  
  tHS<-tH+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)
  #no parar
  if ((lapsTot-floor(dataS$MH))>dataS$MS){
    w2<-paste0("Warning(tHS1): NLaps exceeds expected TyreLife by ", lapsTot - floor(dataS$MH) - dataS$MS," Laps possible with maxTyreLife ", dataS$MaxS-dataS$MS)
  }
  tHS1<-tHS+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH),2)
  #Si parar
  tHS2<-tHS+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1),2)
  
  #3er stint Softs (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tHS2S<-tHS2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2)
  #3er stint Mediums (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tHS2M<-tHS2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2)
  #3er stint Hards (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tHS2H<-tHS2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2)

 
  #RAMA MEDIUMS1ERSTINT
  tMM<-tM+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)
  if (2*floor(dataS$MM)>lapsTot){
    tMM<-tMM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM-2),2)
    tMMH<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)
    tMMS<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)
  }else{
    tMM<-tMM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1),2)
    tMMH<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MM),2)
    tMMS<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MM),2)
  }
  
  tMH<-tM+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)
  #no parar
  if ((lapsTot-floor(dataS$MM))>dataS$MH){
    w3<-paste0("Warning(tMH1): NLaps exceeds expected TyreLife by ", lapsTot - floor(dataS$MM) - dataS$MH," Laps possible with maxTyreLife ", dataS$MaxH-dataS$MH)
  }
  tMH1<-tMH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM),2)
  #Si parar
  tMH2<-tMH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MH-1),2)
  
  #3er stint Softs (mediums 2ndo stint,hards 1er stint,Si parar)
  tMH2S<-tMH2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2)
  #3er stint Mediums (mediums 2ndo stint,hards 1er stint,Si parar)
  tMH2M<-tMH2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2)
  #3er stint Hards (mediums 2ndo stint,hards 1er stint,Si parar)
  tMH2H<-tMH2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2)
  
  tMS<-tM+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)
  #no parar
  if ((lapsTot-floor(dataS$MM))>dataS$MS){
    w4<-paste0("Warning(tMS1): NLaps exceeds expected TyreLife by ", lapsTot - floor(dataS$MM) - dataS$MS," Laps possible with maxTyreLife ", dataS$MaxS-dataS$MS)
  }
  tMS1<-tMS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM),2)
  #Si parar
  tMS2<-tMS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1),2)
  
  #3er stint Softs (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tMS2S<-tMS2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2)
  #3er stint Mediums (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tMS2M<-tMS2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2)
  #3er stint Hards (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tMS2H<-tMS2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2)
  
  
  #RAMA SOFT1ERSTINT
  tSS<-tS+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)
  if (2*floor(dataS$MM)>lapsTot){
    tSS<-tSS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MS-2),2)
    tSSH<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)
    tSSM<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)
  }else{
    tSS<-tSS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1),2)
    tSSH<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MS),2)
    tSSM<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MS),2)
  }
  
  tSH<-tS+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)
  #no parar
  if ((lapsTot-floor(dataS$MS))>dataS$MH){
    w5<-paste0("Warning(tSH1): NLaps exceeds expected TyreLife by ", lapsTot - floor(dataS$MS) - dataS$MH," Laps possible with maxTyreLife ", dataS$MaxH-dataS$MH)
  }
  tSH1<-tSH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MS),2)
  #Si parar
  tSH2<-tSH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1),2)
  
  #3er stint Softs (mediums 2ndo stint,hards 1er stint,Si parar)
  tSH2S<-tSH2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2)
  #3er stint Mediums (mediums 2ndo stint,hards 1er stint,Si parar)
  tSH2M<-tSH2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2)
  #3er stint Hards (mediums 2ndo stint,hards 1er stint,Si parar)
  tSH2H<-tSH2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2)
  
  tSM<-tS+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)
  #no parar
  if ((lapsTot-floor(dataS$MS))>dataS$MM){
    w6<-paste0("Warning(tSM1): NLaps exceeds expected TyreLife by ", lapsTot - floor(dataS$MS) - dataS$MM," Laps possible with maxTyreLife ", dataS$MaxM-dataS$MM)
  }
  tSM1<-tSM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MS),2)
  #Si parar
  tSM2<-tSM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1),2)
  
  #3er stint Softs (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tSM2S<-tSM2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2)
  #3er stint Mediums (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tSM2M<-tSM2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2)
  #3er stint Hards (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tSM2H<-tSM2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2)
  
  
  
  
   res<-data.frame(tHHM=tHHM,
                  tHHS=tHHS,
                  tHM1=tHM1,
                  tHM2S=tHM2S,
                  tHM2M=tHM2M,
                  tHM2H=tHM2H,
                  tHS1=tHS1,
                  tHS2S=tHS2S,
                  tHS2M=tHS2M,
                  tHS2H=tHS2H,
                  #Rama Medium
                  tMMH=tMMH,
                  tMMS=tMMS,
                  tMH1=tMH1,
                  tMH2S=tMH2S,
                  tMH2M=tMH2M,
                  tMH2H=tMH2H,
                  tMS1=tMS1,
                  tMS2S=tMS2S,
                  tMS2M=tMS2M,
                  tMS2H=tMS2H,
                  #Rama HARD
                  tSSH=tSSH,
                  tSSM=tSSM,
                  tSH1=tSH1,
                  tSH2S=tSH2S,
                  tSH2M=tSH2M,
                  tSH2H=tSH2H,
                  tSM1=tSM1,
                  tSM2S=tSM2S,
                  tSM2M=tSM2M,
                  tSM2H=tSM2H,
                  #Warnings
                  w1=w1,
                  w2=w2,
                  w3=w3,
                  w4=w4,
                  w5=w5,
                  w6=w6)
                  
                  
  return(res)

}





Redbull<-modeloDeterminista("Bahrain","Red Bull Racing",stints,nlaps,circuitInfo,model,modelin,modelpit,modelout)
Redbull


McLaren<-modeloDeterminista("China","McLaren",stints,nlaps,circuitInfo,model,modelin,modelpit,modelout)
McLaren

AlphaTauri<-modeloDeterminista("Bahrain","AlphaTauri",stints,nlaps,circuitInfo,model,modelin,modelpit,modelout)
AlphaTauri



lapsTot<-nlaps$Laps[nlaps$GP=="Mexico"]
kmperlap<-circuitInfo$Length[circuitInfo$GP=="Mexico"]
TyreLife<-1
RacePercentage<-1/lapsTot
newdata <- data.frame(TyreLife = 2,
                      RacePercentage = 1 / lapsTot,
                      Compound = "SOFT",
                      Equipo = "Aston Martin",
                      Circuito = "Mexico",
                      Stint = 1)
predict(model,newdata=newdata,type="response")*kmperlap
model$call










modeloVentana<-function(Circuito,Equipo,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout,ventana){
  dataS<-vidapromedio(Circuito, stints)
  print(dataS$MS)
  lapsTot <- nlaps$Laps[nlaps$GP == Circuito]
  #1er Stint
  tH<-tiempoStint(Circuito,"HARD",Equipo,1,nlaps,circuitInfo,modelo,stints,floor(dataS$MH-1+ventana),1)
  tM<-tiempoStint(Circuito,"MEDIUM",Equipo,1,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1+ventana),1)
  tS<-tiempoStint(Circuito,"SOFT",Equipo,1,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1+ventana),1)
  
  #RAMA HARDS1erStint
  tHH<-tH+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)
  if (2*floor(dataS$MH)>lapsTot){
    tHH<-tHH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH-2-ventana),2)
    tHHM<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)
    tHHS<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)
  }else{
    tHH<-tHH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MH-1-ventana),2)
    tHHM<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MH-ventana),2)
    tHHS<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MH-ventana),2)
  }
  
  tHM<-tH+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)
  #no parar
  
  tHM1<-tHM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH),2)
  #Si parar
  tHM2<-tHM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1),2)
  
  #3er stint Softs (mediums 2ndo stint,hards 1er stint,Si parar)
  tHM2S<-tHM2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2)
  #3er stint Mediums (mediums 2ndo stint,hards 1er stint,Si parar)
  tHM2M<-tHM2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2)
  #3er stint Hards (mediums 2ndo stint,hards 1er stint,Si parar)
  tHM2H<-tHM2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2)
  
  tHS<-tH+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)
  #no parar
  
  tHS1<-tHS+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH),2)
  #Si parar
  tHS2<-tHS+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1-ventana),2)
  
  #3er stint Softs (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tHS2S<-tHS2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2)
  #3er stint Mediums (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tHS2M<-tHS2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2)
  #3er stint Hards (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tHS2H<-tHS2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2)
  
  
  #RAMA MEDIUMS1ERSTINT
  tMM<-tM+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)
  if (2*floor(dataS$MM)>lapsTot){
    tMM<-tMM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM-2-ventana),2)
    tMMH<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)
    tMMS<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)
  }else{
    tMM<-tMM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1),2)
    tMMH<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MM-ventana),2)
    tMMS<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MM-ventana),2)
  }
  
  tMH<-tM+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)
  #no parar
 
  tMH1<-tMH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM),2)
  #Si parar
  tMH2<-tMH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MH-1),2)
  
  #3er stint Softs (mediums 2ndo stint,hards 1er stint,Si parar)
  tMH2S<-tMH2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2)
  #3er stint Mediums (mediums 2ndo stint,hards 1er stint,Si parar)
  tMH2M<-tMH2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2)
  #3er stint Hards (mediums 2ndo stint,hards 1er stint,Si parar)
  tMH2H<-tMH2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2)
  
  tMS<-tM+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)
  #no parar
  
  tMS1<-tMS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM),2)
  #Si parar
  tMS2<-tMS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1),2)
  
  #3er stint Softs (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tMS2S<-tMS2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2)
  #3er stint Mediums (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tMS2M<-tMS2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2)
  #3er stint Hards (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tMS2H<-tMS2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2)
  
  
  #RAMA SOFT1ERSTINT
  tSS<-tS+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)
  if (2*floor(dataS$MM)>lapsTot){
    tSS<-tSS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MS-2-ventana),2)
    tSSH<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)
    tSSM<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)
  }else{
    tSS<-tSS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1-ventana),2)
    tSSH<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MS-ventana),2)
    tSSM<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MS-ventana),2)
  }
  
  tSH<-tS+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)
  #no parar
  
  tSH1<-tSH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MS),2)
  #Si parar
  tSH2<-tSH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1),2)
  
  #3er stint Softs (mediums 2ndo stint,hards 1er stint,Si parar)
  tSH2S<-tSH2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2)
  #3er stint Mediums (mediums 2ndo stint,hards 1er stint,Si parar)
  tSH2M<-tSH2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2)
  #3er stint Hards (mediums 2ndo stint,hards 1er stint,Si parar)
  tSH2H<-tSH2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2)
  
  tSM<-tS+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)
  #no parar
  
  tSM1<-tSM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MS),2)
  #Si parar
  tSM2<-tSM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1),2)
  
  #3er stint Softs (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tSM2S<-tSM2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo)+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2)
  #3er stint Mediums (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tSM2M<-tSM2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo)+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2)
  #3er stint Hards (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tSM2H<-tSM2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo)+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2)
  
  
  
  
  res<-data.frame(tHHM=tHHM,
                  tHHS=tHHS,
                  tHM1=tHM1,
                  tHM2S=tHM2S,
                  tHM2M=tHM2M,
                  tHM2H=tHM2H,
                  tHS1=tHS1,
                  tHS2S=tHS2S,
                  tHS2M=tHS2M,
                  tHS2H=tHS2H,
                  #Rama Medium
                  tMMH=tMMH,
                  tMMS=tMMS,
                  tMH1=tMH1,
                  tMH2S=tMH2S,
                  tMH2M=tMH2M,
                  tMH2H=tMH2H,
                  tMS1=tMS1,
                  tMS2S=tMS2S,
                  tMS2M=tMS2M,
                  tMS2H=tMS2H,
                  #Rama HARD
                  tSSH=tSSH,
                  tSSM=tSSM,
                  tSH1=tSH1,
                  tSH2S=tSH2S,
                  tSH2M=tSH2M,
                  tSH2H=tSH2H,
                  tSM1=tSM1,
                  tSM2S=tSM2S,
                  tSM2M=tSM2M,
                  tSM2H=tSM2H)
  
  
  return(res)
  
}

calculaVentana<-function(Circuito,Equipo,stints,nlaps,circuitInfo,model,modelin,modelpit,modelout){
#  res<-list()
#  vent<-seq(1,7,1)
#  for(h in vent){
#    tiempoEsperado<-modeloVentana(Circuito,Equipo,stints,nlaps,circuitInfo,model,modelin,modelpit,modelout,h-4)
#    res[[i]]<-tiempoEsperado
#  }
  tiempoEsperado1<-modeloVentana(Circuito,Equipo,stints,nlaps,circuitInfo,model,modelin,modelpit,modelout,-3)
  tiempoEsperado2<-modeloVentana(Circuito,Equipo,stints,nlaps,circuitInfo,model,modelin,modelpit,modelout,-2)
  tiempoEsperado3<-modeloVentana(Circuito,Equipo,stints,nlaps,circuitInfo,model,modelin,modelpit,modelout,-1)
  tiempoEsperado4<-modeloVentana(Circuito,Equipo,stints,nlaps,circuitInfo,model,modelin,modelpit,modelout,0)
  tiempoEsperado5<-modeloVentana(Circuito,Equipo,stints,nlaps,circuitInfo,model,modelin,modelpit,modelout,1)
  tiempoEsperado6<-modeloVentana(Circuito,Equipo,stints,nlaps,circuitInfo,model,modelin,modelpit,modelout,2)
  tiempoEsperado7<-modeloVentana(Circuito,Equipo,stints,nlaps,circuitInfo,model,modelin,modelpit,modelout,3)
  res<-list(tiempoEsperado1,tiempoEsperado2,tiempoEsperado3,tiempoEsperado4,tiempoEsperado5,tiempoEsperado6,tiempoEsperado7)
 return (res) 
}













minStrat<-function(estrategias){
  names_vector <- c("tHHM", "tHHS", "tHM1", "tHM2S", "tHM2M", "tHM2H", "tHS1", "tHS2S", "tHS2M", "tHS2H",
                    "tMMH", "tMMS", "tMH1", "tMH2S", "tMH2M", "tMH2H", "tMS1", "tMS2S", "tMS2M", "tMS2H",
                    "tSSH", "tSSM", "tSH1", "tSH2S", "tSH2M", "tSH2H", "tSM1", "tSM2S", "tSM2M", "tSM2H")
  min<-matrix(0,nrow=2,ncol=30)
  for (i in 1:30){
    min[1,i]<-as.double(estrategias[[1]][names_vector[i]])
    min[2,i]<--3
    for (j in 2:7){
      if (min>estrategias[[j]][names_vector[i]]){
        min[1,i]<-as.double(estrategias[[j]][names_vector[i]])
        min[2,i]<-j-4
      }
    }
    
    
  }
  colnames(min)<-names_vector
return(min) 
}


min<-McLaren2[[1]]$tHHM
index<-1
for(i in 2:7){
  if (min>McLaren2[[i]]$tHHM){
    min<-McLaren2[[i]]$tHHM
    index<-i
  }
  
} 
min
index





RedBull2<-calculaVentana("Bahrain","Red Bull Racing",stints,nlaps,circuitInfo,model,modelin,modelpit,modelout)
RedBull2
h<-1
a<-modeloVentana("China","McLaren",stints,nlaps,circuitInfo,model,modelin,modelpit,modelout,h-4)

minMcLaren<-minStrat(McLaren2)
minMcLaren


minRedBull<-minStrat(RedBull2)
minRedBull
min(minRedBull[1,])







modeloDeterminista2Stints <- function(Circuito, Equipo, stints, nlaps, circuitInfo, modelo, modelin, modelpit, modelout) {
  dataS <- vidapromedio(Circuito, stints)
  lapsTot <- nlaps$Laps[nlaps$GP == Circuito]
  
  # 1er Stint
  tH <- tiempoStint(Circuito, "HARD", Equipo, 1, nlaps, circuitInfo, modelo, stints, floor(dataS$MH - 1), 1)
  tM <- tiempoStint(Circuito, "MEDIUM", Equipo, 1, nlaps, circuitInfo, modelo, stints, floor(dataS$MM - 1), 1)
  tS <- tiempoStint(Circuito, "SOFT", Equipo, 1, nlaps, circuitInfo, modelo, stints, floor(dataS$MS - 1), 1)
  
  # Estrategia con HARD
  tHM <- tH + pitstopcost(modelin, modelpit, modelout, Circuito, "MEDIUM", circuitInfo)
  tHM1 <- tHM + tiempoStint(Circuito, "MEDIUM", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MH), 2)
  tHS <- tH + pitstopcost(modelin, modelpit, modelout, Circuito, "SOFT", circuitInfo)
  tHS1 <- tHS + tiempoStint(Circuito, "SOFT", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MH), 2)
  
  # Estrategia con MEDIUM
  tMH <- tM + pitstopcost(modelin, modelpit, modelout, Circuito, "HARD", circuitInfo)
  tMH1 <- tMH + tiempoStint(Circuito, "HARD", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MM), 2)
  tMS <- tM + pitstopcost(modelin, modelpit, modelout, Circuito, "SOFT", circuitInfo)
  tMS1 <- tMS + tiempoStint(Circuito, "SOFT", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MM), 2)
  
  # Estrategia con SOFT
  tSH <- tS + pitstopcost(modelin, modelpit, modelout, Circuito, "HARD", circuitInfo)
  tSH1 <- tSH + tiempoStint(Circuito, "HARD", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MS), 2)
  tSM <- tS + pitstopcost(modelin, modelpit, modelout, Circuito, "MEDIUM", circuitInfo)
  tSM1 <- tSM + tiempoStint(Circuito, "MEDIUM", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MS), 2)
  
  res <- data.frame(
    tHM1 = tHM1,
    tHS1 = tHS1,
    tMH1 = tMH1,
    tMS1 = tMS1,
    tSH1 = tSH1,
    tSM1 = tSM1
  )
  
  return(res)
}

modeloventana2stints<-function(Circuito, Equipo, stints, nlaps, circuitInfo, modelo, modelin, modelpit, modelout,ventana) {
  dataS <- vidapromedio(Circuito, stints)
  lapsTot <- nlaps$Laps[nlaps$GP == Circuito]
  
  # 1er Stint
  tH <- tiempoStint(Circuito, "HARD", Equipo, 1, nlaps, circuitInfo, modelo, stints, floor(dataS$MH - 1+ventana), 1)
  tM <- tiempoStint(Circuito, "MEDIUM", Equipo, 1, nlaps, circuitInfo, modelo, stints, floor(dataS$MM - 1+ventana), 1)
  tS <- tiempoStint(Circuito, "SOFT", Equipo, 1, nlaps, circuitInfo, modelo, stints, floor(dataS$MS - 1+ventana), 1)
  
  # Estrategia con HARD
  tHM <- tH + pitstopcost(modelin, modelpit, modelout, Circuito, "MEDIUM", circuitInfo)
  tHM1 <- tHM + tiempoStint(Circuito, "MEDIUM", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MH-ventana), 2)
  tHS <- tH + pitstopcost(modelin, modelpit, modelout, Circuito, "SOFT", circuitInfo)
  tHS1 <- tHS + tiempoStint(Circuito, "SOFT", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MH-ventana), 2)
  
  # Estrategia con MEDIUM
  tMH <- tM + pitstopcost(modelin, modelpit, modelout, Circuito, "HARD", circuitInfo)
  tMH1 <- tMH + tiempoStint(Circuito, "HARD", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MM-ventana), 2)
  tMS <- tM + pitstopcost(modelin, modelpit, modelout, Circuito, "SOFT", circuitInfo)
  tMS1 <- tMS + tiempoStint(Circuito, "SOFT", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MM-ventana), 2)
  
  # Estrategia con SOFT
  tSH <- tS + pitstopcost(modelin, modelpit, modelout, Circuito, "HARD", circuitInfo)
  tSH1 <- tSH + tiempoStint(Circuito, "HARD", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MS-ventana), 2)
  tSM <- tS + pitstopcost(modelin, modelpit, modelout, Circuito, "MEDIUM", circuitInfo)
  tSM1 <- tSM + tiempoStint(Circuito, "MEDIUM", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MS-ventana), 2)
  
  res <- data.frame(
    tHM1 = tHM1,
    tHS1 = tHS1,
    tMH1 = tMH1,
    tMS1 = tMS1,
    tSH1 = tSH1,
    tSM1 = tSM1
  )
  
  return(res)
}


calculaVentanaUnderOver<-function(Circuito,Equipo,stints,nlaps,circuitInfo,model,modelin,modelpit,modelout){
  tiempoEsperado1<-modeloventana2stints(Circuito,Equipo,stints,nlaps,circuitInfo,model,modelin,modelpit,modelout,-3)
  tiempoEsperado2<-modeloventana2stints(Circuito,Equipo,stints,nlaps,circuitInfo,model,modelin,modelpit,modelout,0)
  tiempoEsperado3<-modeloventana2stints(Circuito,Equipo,stints,nlaps,circuitInfo,model,modelin,modelpit,modelout,3)
  res<-list(tiempoEsperado1,tiempoEsperado2,tiempoEsperado3)
  return (res) 
}

minStrat2 <- function(estrategias) {
  names_vector <- c("tHM1", "tHS1", "tMH1", "tMS1", "tSH1", "tSM1")
  min <- matrix(0, nrow = 2, ncol = length(names_vector))
  
  for (i in 1:length(names_vector)) {
    min[1, i] <- estrategias[1, names_vector[i]]
    min[2, i] <- -3
    
    for (j in 2:7) {
      if (min[1, i] > estrategias[j, names_vector[i]]) {
        min[1, i] <- estrategias[j, names_vector[i]]
        min[2, i] <- j - 4
      }
    }
  }
  
  colnames(min) <- names_vector
  return(min) 
}




modeloRival <- function(Circuito, Equipo, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout, Gap, Rival) {
  tiempoED <- calculaVentanaUnderOver(Circuito, Equipo, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout)
  tiempoER <- calculaVentanaUnderOver(Circuito, Rival, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout)
  MEDtiempo<-vector()
  MEDest<-vector()
  MERtiempo<-vector()
  MERest<-vector()
  for (i in 1:3){
    MED<-minEstrategia(tiempoED[[i]]) 
    MEDest[i]<-MED[1]
    MEDtiempo[i]<-as.double(MED[2])
    MER<-minEstrategia(tiempoER[[i]])
    MERest[i]<-MER[1]
    MERtiempo[i]<-as.double(MER[2])
  }
  print(MEDtiempo)
  print(MERtiempo)
compa<-matrix(0,nrow=9,ncol=3)
for (i in 1:3){
  for (j in 1:3){
    compa[3*(i-1)+j,1]<-i
    compa[3*(i-1)+j,2]<-j
    compa[3*(i-1)+j,3]<-MEDtiempo[i]-MERtiempo[j]+Gap
  }
}
print(MEDest)
print(MERest)
 return(compa)
}

minEstrategia <- function(tiempo_vector) {
  nombres_estrategias <- c("tHM1", "tHS1", "tMH1", "tMS1", "tSH1", "tSM1")
  tiempo_minimo <- min(tiempo_vector)
  estrategia_minima <- nombres_estrategias[which(tiempo_vector == tiempo_minimo)]
  return(c(estrategia_minima,tiempo_minimo))
}



McLaren3<-modeloRival("Monaco","McLaren",stints,nlaps,circuitInfo,model,modelin,modelpit,modelout,3,"Ferrari")
McLaren3


tiempoED <- calculaVentanaUnderOver("Australia","McLaren",stints,nlaps,circuitInfo,model,modelin,modelpit,modelout)
minStrat2(tiempoED[1])

min(tiempoED[[2]])
