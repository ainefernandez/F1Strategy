# 4. RESULTS
# This R script generates all the results presented in Chapter 6: 
# "La Línea de Meta: Resultados".
# The evaluation metric used is Accuracy, and the models are tested on four different 2024 Grands Prix: 
# Bahrain, Singapore, Monza, and China.
# The Safety Car model is also tested to predict the probability of a Safety Car occurring during the race.

library(dplyr)
library(ggplot2)

grid_data <- read.csv("Grid.csv")
strategy_data <- read.csv("Strategyfull.csv")
stints <- read.csv("Stints.csv")

merged_data <- merge(grid_data, strategy_data, by = c("Driver", "GP", "Year"))
merged_data <- merged_data[, !colnames(merged_data) %in% c("X.x", "X.y")]
test_data<-merged_data[merged_data$Year %in% c(2024), ]

#Auxiliar functions
findBestStrategies <- function(strategy_results) { 
  best_results <- data.frame(MinTime = numeric(0), Strategy = character(0), stringsAsFactors = FALSE) 
  if (!is.null(strategy_results) && nrow(strategy_results) > 0) { 
    times <- unlist(strategy_results) 
    sorted_indices <- order(times, na.last = NA)  
    best_times <- times[sorted_indices[1:3]]       
    best_strategies <- names(times)[sorted_indices[1:3]]  
    for (j in 1:length(best_times)) { 
      best_results <- rbind(best_results, data.frame(MinTime = best_times[j], Strategy = best_strategies[j])) 
    } 
  } 
  
  return(best_results) 
}


findFastestStrategyByCompound <- function(strategy_times) {
  
  strategies <- colnames(strategy_times)
  times <- as.numeric(strategy_times[1, ])
  
 
  best_results <- data.frame(Compound = character(), Strategy = character(), MinTime = numeric(), stringsAsFactors = FALSE)
  
  
  hard_strategies <- grep("^tH", strategies, value = TRUE)
  medium_strategies <- grep("^tM", strategies, value = TRUE)
  soft_strategies <- grep("^tS", strategies, value = TRUE)
  
  find_fastest <- function(compound_strategies) {
    compound_times <- times[match(compound_strategies, strategies)]
    min_time_index <- which.min(compound_times)
    return(data.frame(Compound = substr(compound_strategies[1], 2, 2), 
                      Strategy = compound_strategies[min_time_index], 
                      MinTime = compound_times[min_time_index]))
  }
  
  
  best_results <- rbind(best_results, find_fastest(hard_strategies))
  best_results <- rbind(best_results, find_fastest(medium_strategies))
  best_results <- rbind(best_results, find_fastest(soft_strategies))
  
  return(best_results)
}

getThreeBestStrategies <- function(data, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout) {
  final_results <- data.frame() 
  
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    Circuito <- row$GP
    Equipo <- row$Team
    Driver <- row$Driver
    Position <- row$Position1lap
    Year <- row$Year
    res <- modeloDeterminista(Circuito, Equipo, Driver, Position, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout)
    
    
    bestStrategies <- findFastestStrategyByCompound(res)
    
    new_row <- data.frame(
      Circuito = Circuito,
      Año = Year,
      Equipo = Equipo,
      Piloto = Driver,
      MejorEstrategiaH = NA,
      MejorEstrategiaHT = NA,
      MejorEstrategiaM = NA,
      MejorEstrategiaMT = NA,
      MejorEstrategiaS = NA,
      MejorEstrategiaST = NA
    )
    
    
    for (j in 1:nrow(bestStrategies)) {
      compound <- bestStrategies$Compound[j]
      strategy <- bestStrategies$Strategy[j]
      time <- bestStrategies$MinTime[j]
      
      if (compound == "H") {
        new_row$MejorEstrategiaH <- strategy
        new_row$MejorEstrategiaHT <- time
      } else if (compound == "M") {
        new_row$MejorEstrategiaM <- strategy
        new_row$MejorEstrategiaMT <- time
      } else if (compound == "S") {
        new_row$MejorEstrategiaS <- strategy
        new_row$MejorEstrategiaST <- time
      }
    }
    
    final_results <- rbind(final_results, new_row)
  }
  
  row.names(final_results) <- NULL
  return(final_results)
}



getAccuracy <- function(resultsTest, estrategias) { 
  
  resultsTest$MejorEstrategiaH <- estrategias[resultsTest$MejorEstrategiaH]
  resultsTest$MejorEstrategiaM <- estrategias[resultsTest$MejorEstrategiaM]
  resultsTest$MejorEstrategiaS <- estrategias[resultsTest$MejorEstrategiaS]
  
  
  resultsTest$Acierto <- with(resultsTest, ifelse(
    EstrategiaReal == MejorEstrategiaH, "HARD",
    ifelse(EstrategiaReal == MejorEstrategiaM, "MEDIUM",
           ifelse(EstrategiaReal == MejorEstrategiaS, "SOFT", "Ninguna")
    )
  ))
  
  
  aciertos <- table(resultsTest$Acierto)
  total <- sum(aciertos, na.rm = TRUE)
  
  
  accuracy_mejor <- ifelse("HARD" %in% names(aciertos), aciertos["HARD"] / total, 0)
  accuracy_segunda <- ifelse("MEDIUM" %in% names(aciertos), aciertos["MEDIUM"] / total, 0)
  accuracy_tercera <- ifelse("SOFT" %in% names(aciertos), aciertos["SOFT"] / total, 0)
  accuracy_general <- (accuracy_mejor + accuracy_segunda + accuracy_tercera)
  
  
  accuracy_df <- data.frame(
    Estrategia = c("HARD", "MEDIUM", "SOFT", "General"),
    Accuracy = c(
      accuracy_mejor,
      accuracy_segunda,
      accuracy_tercera,
      accuracy_general
    ),
    Neumático = c(
      unique(resultsTest$MejorEstrategiaH)[1],  
      unique(resultsTest$MejorEstrategiaM)[1],  
      unique(resultsTest$MejorEstrategiaS)[1],  
      NA  
    )
  )
  
  
  resultsSummary <- resultsTest[, c("Circuito", "Año", "Piloto", "Equipo", "Acierto", "MejorEstrategiaH", "MejorEstrategiaM", "MejorEstrategiaS")]
  resultsSummary$Acc <- ifelse(resultsSummary$Acierto == "Ninguna", 0, 1)
  
  return(list(accuracy_df = accuracy_df, summary_aciertos = resultsSummary))
}

minVentana <- function(data) {
  resultados <- data.frame(min_time = numeric(), min_strategy = character(), ventana = integer())
  
  for (i in 1:7) {
    df <- as.data.frame(data[[i]])
    min_time <- min(unlist(df))
    min_strategy <- names(df)[which.min(unlist(df))]
    ventana <- i
    resultados <- rbind(resultados, data.frame(min_time, min_strategy, ventana))
  }
  
  min_global <- min(resultados$min_time)
  min_global_strategy <- resultados$min_strategy[which.min(resultados$min_time)]
  
  resultados_globales <- list(resultados_individuales = resultados,
                              min_global_time = min_global,
                              min_global_strategy = min_global_strategy)
  
  return(resultados_globales)
}


#Main Function to calculate accuracy, Deterministic model 
analyzeRace <- function(race_data, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout, estrategias) {
  
  raceResults <- getThreeBestStrategies(race_data, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout)
  
  
  raceResults <- cbind(raceResults, race_data$Strategy)
  
  
  names(raceResults) <- c("Circuito", "Año", "Equipo", "Piloto", "MejorEstrategiaH", 
                          "MejorEstrategiaHT", "MejorEstrategiaM", "MejorEstrategiaMT", 
                          "MejorEstrategiaS", "MejorEstrategiaST", "EstrategiaReal")
  
  
  raceAcc <- getAccuracy(raceResults, estrategias)
  
  return(list(raceResults = raceResults, raceAcc = raceAcc))
}

#Main function Window model 
Ventana <- function(data, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout) {
  resultados_por_piloto <- data.frame()
  
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    Circuito <- row$GP
    Equipo <- row$Team
    Driver <- row$Driver
    Position <- row$Position1lap
    Year <- row$Year
    
    res <- calculaVentana(Circuito, Equipo, Driver, Position, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout)
    min_res <- minVentana(res)
    
    
    resultados_por_piloto <- rbind(resultados_por_piloto, 
                                   cbind(data[i, ], 
                                         min_global_time = min_res$min_global_time, 
                                         min_global_strategy = min_res$min_global_strategy, 
                                         ventana = min_res$resultados_individuales$ventana[which.min(min_res$resultados_individuales$min_time)]))
  }

  estrategia_optima_piloto <- resultados_por_piloto %>%
    group_by(Driver) %>%
    slice_min(min_global_time, with_ties = FALSE) %>%
    select(Driver, min_global_time, min_global_strategy, ventana)
  
  return(estrategia_optima_piloto)
}

#RESULTS (Decision Trees Models)

#Get test data for 2024 GP's: Bahrain, Singapore, Monza, China 
Bahrain2024<-test_data[test_data$GP == "Bahrain" & test_data$Year == 2024, ]
Singapore2024<-test_data[test_data$GP == "Singapore" & test_data$Year == 2024, ]
Monza2024<-test_data[test_data$GP == "Monza" & test_data$Year == 2024, ]
China2024<-test_data[test_data$GP == "China" & test_data$Year == 2024, ]

#Deterministic model 
Bahrain2024R<-analyzeRace(Bahrain2024, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout, estrategias)
Singapore2024R<-analyzeRace(Singapore2024, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout, estrategias) 
Monza2024R<-analyzeRace(Monza2024, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout, estrategias)
China2024R<-analyzeRace(China2024, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout, estrategias)

#Window model 
WindowBahrain<-Ventana(Bahrain2024, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout)
WindowSingapore<-Ventana(Singapore2024, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout)
WindowMonza<-Ventana(Monza2024, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout)
WindowChina<-Ventana(China2024, stints, nlaps, circuitInfo, model, modelin, modelpit, modelout)

#RESULTS (Safety Cars)
safetycars<-read.csv("SafetyCars.csv")
risk<-as.data.frame(cbind(SafetyCarByCircuit$RiskCategory,SafetyCarByCircuit$GP))
names(risk)<-c("Risk","GP")
safetycars$SafetyCar <- ifelse(safetycars$Label == "Safety Car", 1, 0)
safetycars<- merge(safetycars,risk, by = "GP", all = TRUE)
safetycars$Risk<-factor(safetycars$Risk)
safetycarsmodel<-glm(SafetyCar~GP+LapNumber, data=safetycars,family=binomial())

lowRisk <- safetycars %>%
  filter(Risk == "Low Risk")


mediumRisk <- safetycars %>%
  filter(Risk == "Medium Risk")


highRisk <- safetycars %>%
  filter(Risk == "High Risk")

#Test data 
safetycars2024<-read.csv("SafetyCars2024.csv")
safetycars2024<- merge(safetycars2024,risk, by = "GP", all = TRUE)

lowRisk2024 <- safetycars2024 %>%
  filter(Risk == "Low Risk")


mediumRisk2024 <- safetycars2024 %>%
  filter(Risk == "Medium Risk")


highRisk2024 <- safetycars2024 %>%
  filter(Risk == "High Risk")

#Predict probability of SC for Low Risk GP's 
safetycarLowRisk <- data.frame()
for (i in 1:nrow(lowRisk2024)) {
  row <- lowRisk2024[i, ]  
  LapNumber <- row$LapNumber  
  GP <- row$GP 
  res2 <- predict(safetycarsmodel, newdata = row, type = "response")
  result <- data.frame(LapNumber = LapNumber, GP = GP, Probability = res2)
  safetycarLowRisk <- rbind(safetycarLowRisk, result)
}




#Predict probability of SC for Medium Risk GP's 
safetycarMediumRisk <- data.frame()
for (i in 1:nrow(mediumRisk2024)) {
  row <- mediumRisk2024[i, ]  
  LapNumber <- row$LapNumber  
  GP <- row$GP  
  res2 <- predict(safetycarsmodel, newdata = row, type = "response")
  result <- data.frame(LapNumber = LapNumber, GP = GP, Probability = res2)
  safetycarMediumRisk <- rbind(safetycarMediumRisk, result)
}

#Predict probability of SC for High Risk GP's 
safetycarHighRisk <- data.frame()
for (i in 1:nrow(highRisk2024)) {
  row <- highRisk2024[i, ]  
  LapNumber <- row$LapNumber  
  GP <- row$GP  
  res2 <- predict(safetycarsmodel, newdata = row, type = "response")
  result <- data.frame(LapNumber = LapNumber, GP = GP, Probability = res2)
  safetycarHighRisk <- rbind(safetycarHighRisk, result)
}



#Merge to calculate Race Percentage 
safetycarLowRisk<-merge(safetycarLowRisk, nlaps, by = "GP")
safetycarMediumRisk<-merge(safetycarMediumRisk, nlaps, by = "GP")
safetycarHighRisk<-merge(safetycarHighRisk, nlaps, by = "GP")

#Calculate Race Percentage
promedio_lowRisk <- safetycarLowRisk %>%
  mutate(RacePercentage = (LapNumber / Laps) * 100) 
promedio_mediumRisk <- safetycarMediumRisk %>%
  mutate(RacePercentage = (LapNumber / Laps) * 100) 
promedio_highRisk <- safetycarHighRisk %>%
  mutate(RacePercentage = (LapNumber / Laps) * 100) 

#Construct a DF with all the averages
promedios_combinados <- bind_rows(promedio_lowRisk, promedio_mediumRisk, promedio_highRisk)
promedios_combinados<-na.omit(promedios_combinados)
promedios_combinados$Risk <- factor(promedios_combinados$Risk, levels = c("Low Risk", "Medium Risk", "High Risk"))
promedios_combinados <- promedios_combinados %>%
  filter(LapNumber != 0)


#Plot the results LapNumber vs AvgProbability 
plotcompleto<-ggplot(promedios_combinados, aes(x = LapNumber, y = AvgProbability, color = Risk)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, by = 0.010))+
  scale_x_continuous(limits = c(0,80), breaks = seq(0, 80, by = 10))+
  labs(title = "Promedio de probabilidades de Safety Car",
       x = "Número de vuelta",
       y = "Probabilidad Promedio",
       color = "Nivel de Riesgo") +  
  scale_color_manual(values = c("Low Risk" = "#5DAE2B", "Medium Risk" = "orange", "High Risk" = "red"),
                     labels = c("Low Risk" = "Riesgo Bajo", 
                                "Medium Risk" = "Riesgo Medio", 
                                "High Risk" = "Riesgo Alto")) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) 


promedio_lowRisk <- na.omit(promedio_lowRisk)
promedio_highRisk<-na.omit(promedio_highRisk)
promedio_mediumRisk<-na.omit(promedio_mediumRisk)

promedio_mediumRisk <- promedio_mediumRisk[!(promedio_mediumRisk$GP %in% c("Qatar", "Abu Dhabi", "Mexico")), ]

promedio_highRisk_ordered <- promedio_highRisk[order(promedio_highRisk$LapNumber == 1, -promedio_highRisk$Probability), ]

#Probability by race percentage (High Risk)
high <- ggplot(promedio_highRisk, aes(x = RacePercentage, y = Probability, color = GP)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, by = 0.01)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5)) +
  labs(title = "Probabilidad de Safety Car por porcentaje de carrera",
       x = "Porcentaje de Carrera (%)",
       y = "Probabilidad",
       subtitle = "Riesgo Alto",
       color = "Circuito") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # Center the title
        plot.subtitle = element_text(hjust = 0.5)) # Center the subtitle

#Probability by race percentage (Medium Risk)
medium <- ggplot(promedio_mediumRisk, aes(x = RacePercentage, y = Probability, color = GP)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, by = 0.01)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5)) +
  labs(title = "Probabilidad de Safety Car por porcentaje de carrera",
       x = "Porcentaje de Carrera (%)",
       y = "Probabilidad",
       subtitle = "Riesgo Medio",
       color = "Circuito") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) 

#Probability by race percentage (Low Risk)
low <- ggplot(promedio_lowRisk, aes(x = RacePercentage, y = Probability, color = GP)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 0.1), breaks = seq(0, 0.2, by = 0.01)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5)) +
  labs(title = "Probabilidad de Safety Car por porcentaje de carrera",
       x = "Porcentaje de Carrera (%)",
       y = "Probabilidad",
       subtitle = "Riesgo Bajo",
       color = "Circuito") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) 






