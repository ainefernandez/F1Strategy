# 3. DECISION TREE MODELS
# This R script implements the decision tree models described in Chapter 5: 
# "Arranca la carrera: Implementación".
# These models use the previously estimated GLM models to predict the optimal strategy, 
# based on estimated StintTime and PitstopCost.
# Three types of models are included:

# 1. Deterministic Model:
#    Assumes that drivers must pit on the lap corresponding to the expected tyre life of their compound.

# 2. Window Model:
#    Allows more flexibility in pit timing, enabling stops slightly earlier or later than the expected tyre life.

# 3. Direct Rival Model:
#    A first step toward a dynamic model. It evaluates the potential for undercut or overcut strategies 
#    by considering the driver's closest rival or the target driver to overtake.



#GLM'S Models
modelLaptime<-glm(LapTimePerKM~GP+RacePercentage+Driver+Team+TyreLife+Compound+Position+Stint, data = data, family = inverse.gaussian)
modelpit<-glm(PitstopT~Circuit, data=datapitstops,family=inverse.gaussian)
modelin<-glm(LapTimePerKM~GP+Compound+TyreLife+Stint, data=datainlaps,family=inverse.gaussian)
modelout<-glm(LapTimePerKM~GP+Compound, data=dataoutlaps,family=inverse.gaussian)

#Auxiliar functions 
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

tiempoStint <- function(Circuito, Compound, Equipo, Stint, nlaps, circuitInfo, modelo,stints,top,low,Driver,Position) {
  dataS <- vidapromedio(Circuito, stints)
  lapsTot <- nlaps$Laps[nlaps$GP == Circuito]
  kmperlap <- circuitInfo$Length[circuitInfo$GP == Circuito]
  TStint <- 0
  for (i in low:top) {
    newdata <- data.frame(TyreLife = i,
                          RacePercentage = i / lapsTot,
                          Compound = Compound,
                          Team = Equipo,
                          Driver=Driver,
                          GP = Circuito,
                          Stint = Stint,
                          Position=Position)
    TStint <- TStint + predict(modelo, newdata = newdata, type = "response") * kmperlap
    
  }
  
  return(TStint)
}


pitstopcost<-function(modelin,modelpit,modelout,Circuito,Compound,circuitInfo,TyreLife,Stint,CompoundIn){
  kmperlap <- circuitInfo$Length[circuitInfo$GP == Circuito]
  newdatain<-data.frame(GP=Circuito,
                        Compound=CompoundIn,
                        TyreLife=TyreLife,
                        Stint=Stint)
  newdatapit<-data.frame(Circuit=Circuito)
  newdataout<-data.frame(GP=Circuito,
                         Compound=Compound)
  inlap<-predict(modelin,newdata=newdatain,type="response")*kmperlap
  outlap<-predict(modelout,newdata=newdataout,type="response")*kmperlap
  pit<-predict(modelpit,newdata=newdatapit,type="response")
  TPit<-inlap+pit+outlap
  return(TPit)
}



# DETERMINISTIC MODEL
modeloDeterminista<-function(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout){
  dataS<-vidapromedio(Circuito, stints)
  lapsTot <- nlaps$Laps[nlaps$GP == Circuito]
  #1er Stint
  tH<-tiempoStint(Circuito,"HARD",Equipo,1,nlaps,circuitInfo,modelo,stints,floor(dataS$MH-1),1,Driver,Position)
  tM<-tiempoStint(Circuito,"MEDIUM",Equipo,1,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1),1,Driver,Position)
  tS<-tiempoStint(Circuito,"SOFT",Equipo,1,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1),1,Driver,Position)
  
  #RAMA HARDS1erStint
  #2ndo Stint (HARD)
  tHH<-tH+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MH-1),1,"HARD")
  if (2*floor(dataS$MH)>lapsTot){
    #Si le da para terminar la carrera, tiene que cambiar al menos 2 vueltas antes 
    #De terminar la carrera, inlap y outlap con el otro compound
    tHH<-tHH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH-2),2,Driver,Position)
    tHHM<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,lapsTot-floor(dataS$MH-2),2,"HARD")
    tHHS<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,lapsTot-floor(dataS$MH-2),2,"HARD")
  }else{
    #Si no le da, para normal 
    tHH<-tHH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MH-1),2,Driver,Position)
    tHHM<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MH-1),2,"HARD")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MH),2,Driver,Position)
    tHHS<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MH-1),2,"HARD")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MH),2,Driver,Position)
  }
  
  #2 Stint (MEDIUM)
  tHM<-tH+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MH-1),1,"HARD")
  #no parar
  if ((lapsTot-floor(dataS$MH))>dataS$MM){
    w1<-paste0("Warning(tHM1): NLaps exceeds expected TyreLife by ", lapsTot - floor(dataS$MH) - dataS$MM," Laps possible with maxTyreLife ", dataS$MaxM-dataS$MM)
  }
  tHM1<-tHM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH),2,Driver,Position)
  
  #Si parar
  tHM2<-tHM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1),2,Driver,Position)
  
  #3er stint Softs (mediums 2ndo stint,hards 1er stint,Si parar)
  tHM2S<-tHM2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MM-1),2,"MEDIUM")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2,Driver,Position)
  #3er stint Mediums (mediums 2ndo stint,hards 1er stint,Si parar)
  tHM2M<-tHM2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MM-1),2,"MEDIUM")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2,Driver,Position)
  #3er stint Hards (mediums 2ndo stint,hards 1er stint,Si parar)
  tHM2H<-tHM2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MM-1),2,"MEDIUM")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2,Driver,Position)
  
  #2 Stint (SOFT)
  tHS<-tH+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MH-1),1,"HARD")
  #no parar
  if ((lapsTot-floor(dataS$MH))>dataS$MS){
    w2<-paste0("Warning(tHS1): NLaps exceeds expected TyreLife by ", lapsTot - floor(dataS$MH) - dataS$MS," Laps possible with maxTyreLife ", dataS$MaxS-dataS$MS)
  }
  tHS1<-tHS+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH),2,Driver,Position)
  
  #Si parar
  tHS2<-tHS+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1),2,Driver,Position)
  
  #3er stint Softs (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tHS2S<-tHS2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MS-1),2,"SOFT")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2,Driver,Position)
  #3er stint Mediums (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tHS2M<-tHS2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MS-1),2,"SOFT")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2,Driver,Position)
  #3er stint Hards (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tHS2H<-tHS2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MS-1),2,"SOFT")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2,Driver,Position)
  
  
  #RAMA MEDIUMS1ERSTINT
  tMM<-tM+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MM-1),1,"MEDIUM")
  
  if (2*floor(dataS$MM)>lapsTot){
    tMM<-tMM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM-2),2,Driver,Position)
    tMMH<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,lapsTot-floor(dataS$MM-2),2,"MEDIUM")
    tMMS<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,lapsTot-floor(dataS$MM-2),2,"MEDIUM")
  }else{
    tMM<-tMM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1),2,Driver,Position)
    tMMH<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MM-1),2,"MEDIUM")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MM),2,Driver,Position)
    tMMS<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MM-1),2,"MEDIUM")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MM),2,Driver,Position)
  }
  #2 Stint (Hard)
  tMH<-tM+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MM-1),1,"MEDIUM")
  #no parar
  if ((lapsTot-floor(dataS$MM))>dataS$MH){
    w3<-paste0("Warning(tMH1): NLaps exceeds expected TyreLife by ", lapsTot - floor(dataS$MM) - dataS$MH," Laps possible with maxTyreLife ", dataS$MaxH-dataS$MH)
  }
  tMH1<-tMH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM),2,Driver,Position)
  
  #Si parar
  tMH2<-tMH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MH-1),2,Driver,Position)
  
  #3er stint Softs (mediums 2ndo stint,hards 1er stint,Si parar)
  tMH2S<-tMH2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MH-1),2,"HARD")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2,Driver,Position)
  #3er stint Mediums (mediums 2ndo stint,hards 1er stint,Si parar)
  tMH2M<-tMH2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MH-1),2,"HARD")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2,Driver,Position)
  #3er stint Hards (mediums 2ndo stint,hards 1er stint,Si parar)
  tMH2H<-tMH2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MH-1),2,"HARD")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2,Driver,Position)
  
  #2 Stint (Soft)
  tMS<-tM+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MM-1),1,"MEDIUM")
  #no parar
  if ((lapsTot-floor(dataS$MM))>dataS$MS){
    w4<-paste0("Warning(tMS1): NLaps exceeds expected TyreLife by ", lapsTot - floor(dataS$MM) - dataS$MS," Laps possible with maxTyreLife ", dataS$MaxS-dataS$MS)
  }
  tMS1<-tMS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM),2,Driver,Position)
  #Si parar
  tMS2<-tMS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1),2,Driver,Position)
  
  #3er stint Softs (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tMS2S<-tMS2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MM-1),2,"SOFT")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2,Driver,Position)
  #3er stint Mediums (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tMS2M<-tMS2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MM-1),2,"SOFT")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2,Driver,Position)
  #3er stint Hards (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tMS2H<-tMS2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MM-1),2,"SOFT")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2,Driver,Position)
  
  
  #RAMA SOFT1ERSTINT
  tSS<-tS+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MS-1),1,"SOFT")
  if (2*floor(dataS$MM)>lapsTot){
    tSS<-tSS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MS-2),2,Driver,Position)
    tSSH<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,lapsTot-floor(dataS$MS-2),2,"SOFT")
    tSSM<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,lapsTot-floor(dataS$MS-2),2,"SOFT")
  }else{
    tSS<-tSS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1),2,Driver,Position)
    tSSH<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MS-1),2,"SOFT")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MS),2,Driver,Position)
    tSSM<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MS-1),2,"SOFT")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MS),2,Driver,Position)
  }
  
  #2 Stint (Hard)
  tSH<-tS+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MS-1),1,"SOFT")
  #no parar
  if ((lapsTot-floor(dataS$MS))>dataS$MH){
    w5<-paste0("Warning(tSH1): NLaps exceeds expected TyreLife by ", lapsTot - floor(dataS$MS) - dataS$MH," Laps possible with maxTyreLife ", dataS$MaxH-dataS$MH)
  }
  tSH1<-tSH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MS),2,Driver,Position)
  #Si parar
  tSH2<-tSH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1),2,Driver,Position)
  
  #3er stint Softs (mediums 2ndo stint,hards 1er stint,Si parar)
  tSH2S<-tSH2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MS-1),2,"HARD")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2,Driver,Position)
  #3er stint Mediums (mediums 2ndo stint,hards 1er stint,Si parar)
  tSH2M<-tSH2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MS-1),2,"HARD")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2,Driver,Position)
  #3er stint Hards (mediums 2ndo stint,hards 1er stint,Si parar)
  tSH2H<-tSH2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MS-1),2,"HARD")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2,Driver,Position)
  
  #2 Stint (Medium)
  tSM<-tS+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MS-1),1,"SOFT")
  #no parar
  if ((lapsTot-floor(dataS$MS))>dataS$MM){
    w6<-paste0("Warning(tSM1): NLaps exceeds expected TyreLife by ", lapsTot - floor(dataS$MS) - dataS$MM," Laps possible with maxTyreLife ", dataS$MaxM-dataS$MM)
  }
  tSM1<-tSM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MS),2,Driver,Position)
  #Si parar
  tSM2<-tSM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1),2,Driver,Position)
  
  #3er stint Softs (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tSM2S<-tSM2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MS-1),2,"MEDIUM")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2,Driver,Position)
  #3er stint Mediums (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tSM2M<-tSM2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MS-1),2,"MEDIUM")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2,Driver,Position)
  #3er stint Hards (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tSM2H<-tSM2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MS-1),2,"MEDIUM")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2,Driver,Position)
  
  
  
  
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


# WINDOW MODEL
modeloVentana<-function(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout,ventana){
  dataS<-vidapromedio(Circuito, stints)
  lapsTot <- nlaps$Laps[nlaps$GP == Circuito]
  #1er Stint
  tH<-tiempoStint(Circuito,"HARD",Equipo,1,nlaps,circuitInfo,modelo,stints,floor(dataS$MH-1+ventana),1,Driver,Position)
  tM<-tiempoStint(Circuito,"MEDIUM",Equipo,1,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1+ventana),1,Driver,Position)
  tS<-tiempoStint(Circuito,"SOFT",Equipo,1,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1+ventana),1,Driver,Position)
  
  #RAMA HARDS1erStint
  tHH<-tH+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MH-1+ventana),1,"HARD")
  if (2*floor(dataS$MH)>lapsTot){
    tHH<-tHH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH-2-ventana),2,Driver,Position)
    tHHM<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,lapsTot-floor(dataS$MH-2-ventana),2,"HARD")
    tHHS<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,lapsTot-floor(dataS$MH-2-ventana),2,"HARD")
  }else{
    tHH<-tHH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MH-1-ventana),2,Driver,Position)
    tHHM<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MH-1-ventana),2,"HARD")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MH-ventana),2,Driver,Position)
    tHHS<-tHH+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MH-1-ventana),2,"HARD")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MH-ventana),2,Driver,Position)
  }
  #2 STINT MEDIUM
  tHM<-tH+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MH-1+ventana),1,"HARD")
  
  #no parar
  tHM1<-tHM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH),2,Driver,Position)
  
  #Si parar
  tHM2<-tHM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1-ventana),2,Driver,Position)
  
  #3er stint Softs (mediums 2ndo stint,hards 1er stint,Si parar)
  tHM2S<-tHM2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MM-1-ventana),2,"MEDIUM")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2,Driver,Position)
  #3er stint Mediums (mediums 2ndo stint,hards 1er stint,Si parar)
  tHM2M<-tHM2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MM-1-ventana),2,"MEDIUM")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2,Driver,Position)
  #3er stint Hards (mediums 2ndo stint,hards 1er stint,Si parar)
  tHM2H<-tHM2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MM-1-ventana),2,"MEDIUM")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2,Driver,Position)
  
  #2 STINT SOFT
  tHS<-tH+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MH-1+ventana),1,"HARD")
  #no parar
  tHS1<-tHS+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH),2,Driver,Position)
  
  #Si parar
  tHS2<-tHS+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1-ventana),2,Driver,Position)
  
  #3er stint Softs (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tHS2S<-tHS2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MS-1-ventana),2,"SOFT")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2,Driver,Position)
  #3er stint Mediums (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tHS2M<-tHS2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MS-1-ventana),2,"SOFT")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2,Driver,Position)
  #3er stint Hards (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tHS2H<-tHS2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MS-1-ventana),2,"SOFT")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2,Driver,Position)
  
  
  #RAMA MEDIUMS1ERSTINT
  tMM<-tM+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MM-1+ventana),1,"MEDIUM")
  if (2*floor(dataS$MM)>lapsTot){
    tMM<-tMM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM-2-ventana),2,Driver,Position)
    tMMH<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,lapsTot-floor(dataS$MM-2-ventana),2,"MEDIUM")
    tMMS<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,lapsTot-floor(dataS$MM-2-ventana),2,"MEDIUM")
  }else{
    tMM<-tMM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1),2,Driver,Position)
    tMMH<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MH-1-ventana),2,"MEDIUM")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MM-ventana),2,Driver,Position)
    tMMS<-tMM+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MH-1-ventana),2,"MEDIUM")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MM-ventana),2,Driver,Position)
  }
  #2 STINT HARD
  tMH<-tM+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MM-1+ventana),1,"MEDIUM")
  #no parar
  
  tMH1<-tMH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM),2,Driver,Position)
  #Si parar
  tMH2<-tMH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MH-1-ventana),2,Driver,Position)
  
  #3er stint Softs (mediums 2ndo stint,hards 1er stint,Si parar)
  tMH2S<-tMH2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MH-1-ventana),2,"HARD")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2,Driver,Position)
  #3er stint Mediums (mediums 2ndo stint,hards 1er stint,Si parar)
  tMH2M<-tMH2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MH-1-ventana),2,"HARD")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2,Driver,Position)
  #3er stint Hards (mediums 2ndo stint,hards 1er stint,Si parar)
  tMH2H<-tMH2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MH-1-ventana),2,"HARD")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MM),2,Driver,Position)
  
  #2 STINT SOFT
  tMS<-tM+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MM-1+ventana),1,"MEDIUM")
  #no parar
  
  tMS1<-tMS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM),2,Driver,Position)
  #Si parar
  tMS2<-tMS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MM-1-ventana),2,Driver,Position)
  
  #3er stint Softs (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tMS2S<-tMS2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MM-1+ventana),1,"SOFT")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2,Driver,Position)
  #3er stint Mediums (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tMS2M<-tMS2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MM-1+ventana),1,"SOFT")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2,Driver,Position)
  #3er stint Hards (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tMS2H<-tMS2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MM-1+ventana),1,"SOFT")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2,Driver,Position)
  
  
  #RAMA SOFT1ERSTINT
  tSS<-tS+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MS-1+ventana),1,"SOFT")
  if (2*floor(dataS$MM)>lapsTot){
    tSS<-tSS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MS-2-ventana),2,Driver,Position)
    tSSH<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,lapsTot-floor(dataS$MS-2-ventana),2,"SOFT")
    tSSM<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,lapsTot-floor(dataS$MS-2-ventana),2,"SOFT")
  }else{
    tSS<-tSS+tiempoStint(Circuito,"SOFT",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1-ventana),2,Driver,Position)
    tSSH<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MS-1-ventana),2,"SOFT")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MS-ventana),2,Driver,Position)
    tSSM<-tSS+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MS-1-ventana),2,"SOFT")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-2*floor(dataS$MS-ventana),2,Driver,Position)
  }
  #2 STINT HARD
  tSH<-tS+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MS-1+ventana),1,"SOFT")
  #no parar
  tSH1<-tSH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MS),2,Driver,Position)
  #Si parar
  tSH2<-tSH+tiempoStint(Circuito,"HARD",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1-ventana),2,Driver,Position)
  
  #3er stint Softs (mediums 2ndo stint,hards 1er stint,Si parar)
  tSH2S<-tSH2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MS-1-ventana),2,"HARD")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2,Driver,Position)
  #3er stint Mediums (mediums 2ndo stint,hards 1er stint,Si parar)
  tSH2M<-tSH2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MS-1-ventana),2,"HARD")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2,Driver,Position)
  #3er stint Hards (mediums 2ndo stint,hards 1er stint,Si parar)
  tSH2H<-tSH2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MS-1-ventana),2,"HARD")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MH)-floor(dataS$MS),2,Driver,Position)
  
  #2 STINT MEDIUMS
  tSM<-tS+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MS-1+ventana),1,"SOFT")
  #no parar
  tSM1<-tSM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MS),2,Driver,Position)
  #Si parar
  tSM2<-tSM+tiempoStint(Circuito,"MEDIUM",Equipo,2,nlaps,circuitInfo,modelo,stints,floor(dataS$MS-1-ventana),2,Driver,Position)
  
  #3er stint Softs (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tSM2S<-tSM2+pitstopcost(modelin,modelpit,modelout,Circuito,"SOFT",circuitInfo,floor(dataS$MS-1-ventana),2,"MEDIUM")+tiempoStint(Circuito,"SOFT",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2,Driver,Position)
  #3er stint Mediums (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tSM2M<-tSM2+pitstopcost(modelin,modelpit,modelout,Circuito,"MEDIUM",circuitInfo,floor(dataS$MS-1-ventana),2,"MEDIUM")+tiempoStint(Circuito,"MEDIUM",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2,Driver,Position)
  #3er stint Hards (SOFTS 2ndo stint,hards 1er stint,Si parar)
  tSM2H<-tSM2+pitstopcost(modelin,modelpit,modelout,Circuito,"HARD",circuitInfo,floor(dataS$MS-1-ventana),2,"MEDIUM")+tiempoStint(Circuito,"HARD",Equipo,3,nlaps,circuitInfo,modelo,stints,lapsTot-floor(dataS$MM)-floor(dataS$MS),2,Driver,Position)
  
  
  
  
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

#FUNCTION THAT CALCULATES THE OPTIMAL WINDOW 
calculaVentana<-function(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout){
  tiempoEsperado1<-modeloVentana(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout,-3)
  tiempoEsperado2<-modeloVentana(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout,-2)
  tiempoEsperado3<-modeloVentana(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout,-1)
  tiempoEsperado4<-modeloVentana(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout,0)
  tiempoEsperado5<-modeloVentana(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout,1)
  tiempoEsperado6<-modeloVentana(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout,2)
  tiempoEsperado7<-modeloVentana(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout,3)
  res<-list(tiempoEsperado1,tiempoEsperado2,tiempoEsperado3,tiempoEsperado4,tiempoEsperado5,tiempoEsperado6,tiempoEsperado7)
  return (res) 
}

# DETERMINISTIC MODEL 2 STINTS
modeloDeterminista2Stints <- function(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout) {
  dataS <- vidapromedio(Circuito, stints)
  lapsTot <- nlaps$Laps[nlaps$GP == Circuito]
  
  # 1er Stint
  tH <- tiempoStint(Circuito, "HARD", Equipo, 1, nlaps, circuitInfo, modelo, stints, floor(dataS$MH - 1), 1,Driver,Position)
  tM <- tiempoStint(Circuito, "MEDIUM", Equipo, 1, nlaps, circuitInfo, modelo, stints, floor(dataS$MM - 1), 1,Driver,Position)
  tS <- tiempoStint(Circuito, "SOFT", Equipo, 1, nlaps, circuitInfo, modelo, stints, floor(dataS$MS - 1), 1,Driver,Position)
  
  # Estrategia con HARD
  tHM <- tH + pitstopcost(modelin, modelpit, modelout, Circuito, "MEDIUM", circuitInfo,floor(dataS$MH - 1),1,"HARD")
  tHM1 <- tHM + tiempoStint(Circuito, "MEDIUM", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MH), 2,Driver,Position)
  tHS <- tH + pitstopcost(modelin, modelpit, modelout, Circuito, "SOFT", circuitInfo,floor(dataS$MH - 1),1,"HARD")
  tHS1 <- tHS + tiempoStint(Circuito, "SOFT", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MH), 2,Driver,Position)
  
  # Estrategia con MEDIUM
  tMH <- tM + pitstopcost(modelin, modelpit, modelout, Circuito, "HARD", circuitInfo,floor(dataS$MM - 1),1,"MEDIUM")
  tMH1 <- tMH + tiempoStint(Circuito, "HARD", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MM), 2,Driver,Position)
  tMS <- tM + pitstopcost(modelin, modelpit, modelout, Circuito, "SOFT", circuitInfo,floor(dataS$MM - 1),1,"MEDIUM")
  tMS1 <- tMS + tiempoStint(Circuito, "SOFT", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MM), 2,Driver,Position)
  
  # Estrategia con SOFT
  tSH <- tS + pitstopcost(modelin, modelpit, modelout, Circuito, "HARD", circuitInfo,floor(dataS$MS - 1),1,"SOFT")
  tSH1 <- tSH + tiempoStint(Circuito, "HARD", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MS), 2,Driver,Position)
  tSM <- tS + pitstopcost(modelin, modelpit, modelout, Circuito, "MEDIUM", circuitInfo,floor(dataS$MS - 1),1,"SOFT")
  tSM1 <- tSM + tiempoStint(Circuito, "MEDIUM", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MS), 2,Driver,Position)
  
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

# WINDOW MODEL 2 STINTS
modeloventana2stints<-function(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout,ventana) {
  dataS <- vidapromedio(Circuito, stints)
  lapsTot <- nlaps$Laps[nlaps$GP == Circuito]
  
  # 1er Stint
  tH <- tiempoStint(Circuito, "HARD", Equipo, 1, nlaps, circuitInfo, modelo, stints, floor(dataS$MH - 1+ventana), 1,Driver,Position)
  tM <- tiempoStint(Circuito, "MEDIUM", Equipo, 1, nlaps, circuitInfo, modelo, stints, floor(dataS$MM - 1+ventana), 1,Driver,Position)
  tS <- tiempoStint(Circuito, "SOFT", Equipo, 1, nlaps, circuitInfo, modelo, stints, floor(dataS$MS - 1+ventana), 1,Driver,Position)
  
  # Estrategia con HARD
  tHM <- tH + pitstopcost(modelin, modelpit, modelout, Circuito, "MEDIUM", circuitInfo,floor(dataS$MH - 1+ventana), 1,"HARD")
  tHM1 <- tHM + tiempoStint(Circuito, "MEDIUM", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MH-ventana), 2,Driver,Position)
  tHS <- tH + pitstopcost(modelin, modelpit, modelout, Circuito, "SOFT", circuitInfo,floor(dataS$MH - 1+ventana), 1,"HARD")
  tHS1 <- tHS + tiempoStint(Circuito, "SOFT", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MH-ventana), 2,Driver,Position)
  
  # Estrategia con MEDIUM
  tMH <- tM + pitstopcost(modelin, modelpit, modelout, Circuito, "HARD", circuitInfo,floor(dataS$MH - 1+ventana), 1,"MEDIUM")
  tMH1 <- tMH + tiempoStint(Circuito, "HARD", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MM-ventana), 2,Driver,Position)
  tMS <- tM + pitstopcost(modelin, modelpit, modelout, Circuito, "SOFT", circuitInfo,floor(dataS$MH - 1+ventana), 1,"MEDIUM")
  tMS1 <- tMS + tiempoStint(Circuito, "SOFT", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MM-ventana), 2,Driver,Position)
  
  # Estrategia con SOFT
  tSH <- tS + pitstopcost(modelin, modelpit, modelout, Circuito, "HARD", circuitInfo,floor(dataS$MH - 1+ventana), 1,"SOFT")
  tSH1 <- tSH + tiempoStint(Circuito, "HARD", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MS-ventana), 2,Driver,Position)
  tSM <- tS + pitstopcost(modelin, modelpit, modelout, Circuito, "MEDIUM", circuitInfo,floor(dataS$MH - 1+ventana), 1,"SOFT")
  tSM1 <- tSM + tiempoStint(Circuito, "MEDIUM", Equipo, 2, nlaps, circuitInfo, modelo, stints, lapsTot - floor(dataS$MS-ventana), 2,Driver,Position)
  
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

# UNDERCUT/OVERCUT
calculaVentanaUnderOver<-function(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout){
  tiempoEsperado1<-modeloventana2stints(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout,-3)
  tiempoEsperado2<-modeloventana2stints(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout,0)
  tiempoEsperado3<-modeloventana2stints(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout,3)
  res<-list(tiempoEsperado1,tiempoEsperado2,tiempoEsperado3)
  return (res) 
}

# DIRECT RIVAL MODEL 
modeloRival <- function(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout, Gap, Rival,DriverR,PositionR) {
  tiempoED <- calculaVentanaUnderOver(Circuito,Equipo,Driver,Position,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout)
  tiempoER <- calculaVentanaUnderOver(Circuito,Rival,DriverR,PositionR,stints,nlaps,circuitInfo,modelo,modelin,modelpit,modelout)
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








