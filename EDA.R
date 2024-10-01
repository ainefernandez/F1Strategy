library(ggplot2)
library(gtools)
library(dplyr)

#setwd("/Users/ainefernandez/documents/F1DataScience")

#Data 
laps<-read.csv("TesisDryQuickLaps2.csv")
circuitInfo<-read.csv("TesisCircuitInfo.csv")
stints<-read.csv("TesisStints2.csv")
pitstops<-read.csv("TesisPitstopsWithTeams.csv")
outlaps<-read.csv("TesisOutlaps2.csv")
inlaps<-read.csv("TesisInlaps2.csv")
nlaps<-read.csv("TesisNLaps.csv")
safetycars<-read.csv("TesisSafetyCars.csv")
strategy<-read.csv("TesisStrategyfull.csv")


drivercolors <- c("VER" = "#3671C6","PER" = "#3671C6","ALO" = "#358C75","SAI" = "#F91536","HAM" = "#6CD3BF",
                  "STR" = "#358C75","RUS" = "#6CD3BF","BOT" = "#C92D4B","GAS" = "#FF66C4","ALB" = "#37BEDD",
                  "TSU" = "#5E8FAA","SAR" = "#37BEDD","MAG" = "#B6BABD","DEV" = "#5E8FAA","HUL" = "#B6BABD",
                  "ZHO" = "#C92D4B", "NOR" = "#F58020","OCO" = "#FF66C4","LEC" = "#F91536","PIA" = "#F58020", "RIC"="#5E8FAA","LAW"="#5E8FAA","BEA"="#F91536")
teamscolors<-c("Red Bull Racing"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
               "Alpine"="#FF66C4","Haas F1 Team"="#B6BABD","RB"="#0335D3","McLaren"="#F58020","Kick Sauber"="#04C404","AlphaTauri"="#5E8FAA","Alfa Romeo"="#C92D4B")

#Teams vs drivers laptimes 
library(ggplot2)

# Create the ggplot boxplot with custom y-axis scale and legend title
ggplot(laps, aes(x = reorder(Team, LapTimePerKM, FUN = function(x) mean(x, na.rm = TRUE)), y = LapTimePerKM, fill = Team)) +
  geom_violin() +
  scale_fill_manual(values = teamscolors) +  # Replace 'teamscolors' with your color vector for teams
  scale_y_continuous(breaks = seq(14, 26, by = 2), limits = c(14, 26)) +  # Custom y-axis scale
  labs(title = "Distribución de tiempos de vuelta por equipo",
       x = "Equipo",
       y = "Tiempo de vuelta por KM (segundos)",
       subtitle = "(2019-2023)") +
  guides(fill = guide_legend(title = "Equipo")) +  # Set legend title
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5))+
  theme(legend.position = "none") 


# Define the list of 2023 drivers
drivers_2023 <- c("VER", "PER", "ALO", "SAI", "HAM", "STR", "RUS", "BOT", "GAS", 
                  "ALB", "TSU", "SAR", "MAG", "HUL", "ZHO", "NOR", "OCO", 
                  "LEC", "PIA", "RIC")

# Filter laps for only drivers in the 2023 grid across 2019-2023
laps_2023_drivers <- subset(laps, Driver %in% drivers_2023)

# Create the ggplot boxplot with custom y-axis scale for 2023 drivers' laps across 2019-2023
ggplot(laps_2023_drivers, aes(x = reorder(Driver, LapTimePerKM, FUN = function(x) mean(x, na.rm = TRUE)), y = LapTimePerKM, fill = Driver)) +
  geom_violin() +
  scale_fill_manual(values = drivercolors) +
  scale_y_continuous(breaks = seq(14, 26, by = 2), limits = c(14, 26)) +  # Custom y-axis scale
  labs(title = "Distribución de tiempos de vuelta por piloto (2019-2023)",
       x = "Piloto",
       y = "Tiempo de vuelta por KM (segundos)",
       subtitle = "Pilotos de la parrilla 2023") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        plot.title = element_text(size = 13, hjust = 0.5),  # Center and size title
        plot.subtitle = element_text(size = 11, hjust = 0.5),  # Center and size subtitle
        legend.position = "none")  # Remove the legend

#CCOMPOUND

library(ggplot2)

# Create the ggplot boxplot sorted by mean LapTimePerKM for each compound with corrected color scale
ggplot(laps, aes(x = reorder(Compound, LapTimePerKM, FUN = function(x) mean(x, na.rm = TRUE)), y = LapTimePerKM, fill = Compound)) +
  geom_violin() +
  scale_fill_manual(values = c("SOFT"="#FF3333", "MEDIUM"="#ffe541", "HARD"="#d2d2d2")) +  # Corrected color scale
  scale_y_continuous(breaks = seq(14, 26, by = 2), limits = c(14, 26)) +  # Custom y-axis scale
  labs(title = "Distribución de tiempos de vuelta por kilómetro por compuesto",
       x = "Compuesto",
       y = "Tiempo de vuelta por kilómetro (segundos)",
       subtitle = "(2019-2023)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        plot.title = element_text(size = 13, hjust = 0.5),  # Center and size title
        plot.subtitle = element_text(size = 11, hjust = 0.5),  # Center and size subtitle
        legend.position = "none")  # Remove the legend

lap_counts <- laps %>%
  group_by(Compound) %>%
  summarise(LapCount = n(), .groups = 'drop')

ggplot(lap_counts, aes(x = reorder(Compound, LapCount), y = LapCount, fill = Compound)) +
  geom_bar(stat = "identity",color="black") +
  scale_fill_manual(values = c("SOFT"="#FF3333", "MEDIUM"="#ffe541", "HARD"="#d2d2d2")) +
  scale_y_continuous(expand = c(0,0),limits = c(0,30000),breaks = seq(0, 30000, by = 5000))+
  labs(title = "Número de vueltas rápidas por compuesto",
       x = "Compuesto",
       y = "Número de vueltas rápidas",
       subtitle = "(2019-2023)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        plot.title = element_text(size = 18, hjust = 0.5),  # Center and size title
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        legend.position = "none")  # Center and size subtitle

average_lap_times <- laps %>%
  group_by(Compound) %>%
  summarise(AvgLapTimePerKM = mean(LapTimePerKM, na.rm = TRUE), .groups = 'drop')

fastest_lap_time <- min(average_lap_times$AvgLapTimePerKM)
average_lap_times <- average_lap_times %>%
  mutate(GapToFastest = AvgLapTimePerKM - fastest_lap_time)

ggplot(average_lap_times, aes(x = reorder(Compound, GapToFastest), y = GapToFastest, fill = Compound)) +
  geom_bar(stat = "identity",color="black") +
  scale_y_continuous(expand = c(0,0),limits = c(0,.1),breaks = seq(0,.1, by = .025))+
  scale_fill_manual(values = c("SOFT"="#FF3333", "MEDIUM"="#ffe541", "HARD"="#d2d2d2")) +  # Custom colors
  labs(title = "Diferencia promedio entre compuestos",
       x = "Compuesto",
       y = "Diferencia promedio al más rápido (segundos)",
       subtitle = "(2019-2023)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        plot.title = element_text(size = 18, hjust = 0.5),  # Center and size title
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        legend.position = "none")  # Center and size subtitle

average_tyre_life <- laps %>%
  group_by(Compound) %>%
  summarise(AvgTyreLife = mean(TyreLife, na.rm = TRUE), .groups = 'drop')

ggplot(average_tyre_life, aes(x = reorder(Compound, AvgTyreLife), y = AvgTyreLife, fill = Compound)) +
  geom_bar(stat = "identity",color="black") +
  scale_y_continuous(expand = c(0,0),limits = c(0,20),breaks = seq(0,20, by = 2))+
  scale_fill_manual(values = c("SOFT"="#FF3333", "MEDIUM"="#ffe541", "HARD"="#d2d2d2")) +  # Custom colors
  labs(title = "Promedio de vida útil de los neumáticos por compuesto",
       x = "Compuesto",
       y = "Vida útil promedio (vueltas)",
       subtitle = "(2019-2023)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        plot.title = element_text(size = 18, hjust = 0.5),  # Center and size title
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        legend.position = "none")  # Center and size subtitle

pitstops$PitstopT<-as.numeric(pitstops$PitstopT)
ggplot(pitstops, aes(x = reorder(Team, PitstopT, FUN = function(x) mean(x, na.rm = TRUE)), y = PitstopT, fill = Team)) +
  geom_violin() +
  scale_fill_manual(values = teamscolors) +  # Replace 'teamscolors' with your color vector for teams
  scale_y_continuous(breaks = seq(14, 26, by = 2), limits = c(14, 26)) +  # Custom y-axis scale
  labs(title = "Distribución de tiempos de vuelta por equipo",
       x = "Equipo",
       y = "Tiempo de vuelta por KM (segundos)",
       subtitle = "(2019-2023)") +
  guides(fill = guide_legend(title = "Equipo")) +  # Set legend title
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5))+
  theme(legend.position = "none") 
