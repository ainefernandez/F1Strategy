# 🏎️ F1 Race Strategy with GLMs and Decision Trees

Hi there! 👋  

Welcome to the repo for my undergraduate thesis in Data Science — a deep dive into Formula 1 race strategies using Generalized Linear Models (GLMs) and Decision Trees.  

📘 If you speak Spanish, you can check out my full thesis here: [Tesis](Tesis.pdf)  

Otherwise, stick around! This README will walk you through all the details of the project.

If you're into statistics, data science, and Formula 1 (just like me 🏁), you're in for a treat!

## 💡 Motivation

Ever since I became a Formula 1 fan, the data side of the sport has fascinated me the most — especially how teams use it to better understand their cars and gain a competitive edge. What truly captivates me is race strategy, a crucial factor that often determines the outcome of a race.

Finding the optimal race strategy is one of the most complex challenges F1 teams face every Grand Prix weekend. So I asked myself: *What if I could use past race data and predictive models to identify the optimal strategy? Could data science help engineers tackle this problem in a new way?*

Of course, F1 teams already use sophisticated models for this (and many other) challenges — but I wanted to approach it from a different angle, using techniques like GLMs and decision trees.

And that’s how the idea for this thesis came to life 🚀

## 📊 Data

First things first — it wouldn’t be a Data Science thesis without data!

The journey began by extracting data from the F1 API using the awesome [FastF1](https://docs.fastf1.dev/) Python library, which provides access to detailed session data from every Grand Prix weekend.

The [Jupyter Notebook](0.DataExtraction.ipynb) handles the extraction of all the key datasets used in this thesis, including:

- Lap times  
- Circuit information  
- Tyre strategies  
- Pitstops  
- Inlaps & Outlaps  
- Safety Cars  

Once the data was collected, it was saved into CSV files for further statistical analysis — most of which was done in R.

The data spans from 2019 to 2024, up until the 2024 Singapore GP. I used 2019-2023 data for training models, and 2024 data for testing.

Next, I’ll explain the variables included in each of these datasets.


### 🏁 Circuit Info

The [CircuitInfo.csv](CircuitInfo.csv) file contains track characteristics, including:

- Country of the Circuit (GP)
- Circuit length
- Level of abrasion
- Level of traction
- Level of braking
- Level of track evolution (TrackEvo)
- Level of grip
- Level of lateral forces (Lateral)
- Level of downforce
- Level of tyre stress (TyreStress)

This data was obtained from Pirelli, the official and only current tyre supplier for Formula 1.

### 🏁 NLaps

The [NLaps.csv](NLaps.csv) file contains the **total number of race laps** for each circuit.  

This information is essential for calculating variables like **RacePercentage**. 

### 🏎️ Dry Quick Laps

The [DryQuickLaps.csv](DryQuickLaps.csv) file contains data from laps driven in **dry conditions** only. Races affected by rain were excluded to maintain consistency in tyre performance and strategy evaluation. Additionally, only **competitive laps** were kept, filtered according to Formula 1’s 107% rule — meaning each lap had to be within 107% of the fastest lap. This dataset contains **64,516 lap records**.

It includes the following variables:

- **Driver** – Abbreviation of the driver’s name (e.g., "VER", "HAM")  
- **Team** – Team the driver was racing for  
- **LapNumber** – Lap in which the time was set  
- **LapTime** – Total time to complete the lap  
- **Stint** – The stint number (i.e., which tyre set this lap belongs to)  
- **Compound** – Tyre compound used (Soft, Medium, Hard)  
- **TyreLife** – Number of laps the tyres had been used at the time of the lap  
- **Position** – Driver’s position on track during the lap  
- **Year** – Season in which the lap was recorded  
- **GP** – Grand Prix where the lap occurred  
- **Length** – Length of the circuit (in km)  
- **Abrasion** – Level of track abrasion  
- **Traction** – Amount of traction required by the circuit  
- **Braking** – Braking demand of the circuit  
- **TrackEvo** – Track evolution across the weekend  
- **Grip** – General grip level of the surface  
- **Lateral** – Lateral forces experienced in the circuit  
- **Downforce** – Level of aerodynamic downforce required  
- **TyreStress** – Tyre stress caused by the circuit layout  
- **LapTimePerKM** – Standardized lap time (lap time divided by circuit length)  
- **Laps** – Total number of race laps for that GP  
- **RacePercentage** – Percentage of the race completed when the lap occurred

### 🔁 Stints

The [Stints.csv](Stints.csv) file contains data on **stints**, which refer to the continuous number of laps a driver completes on the same set of tyres before making a pit stop.

It includes:

- **Driver**: Name of the driver  
- **Stint**: Stint number (e.g., 1st, 2nd, etc.) during the race  
- **Compound**: Tyre compound used in the stint (SOFT, MEDIUM, HARD)  
- **GP**: Grand Prix name  
- **Year**: Year of the event  
- **StintLength**: Number of laps in the stint

### 🏎️ Strategy

The [Strategyfull.csv](Strategyfull.csv) contains detailed data about the tyre strategies used by drivers during a Grand Prix main race.

The file includes the following variables:

- **Year**: The year in which the Grand Prix took place.
- **GP**: The name of the Grand Prix.
- **Driver**: The abbreviation of the driver's name (e.g., "VER" for Max Verstappen, "HAM" for Lewis Hamilton).
- **Strategy**: The sequence of tyre compounds used by the driver throughout the race (e.g., "MEDIUM-MEDIUM-HARD").
- **PitStops**: The total number of pit stops made by the driver during the race.
- **Stint**: The continuous laps driven on a single set of tyres.
- **Compound**: The type of tyre used during the stint (e.g., Soft, Medium, Hard).
- **StintLength**: The number of laps completed in a particular stint.
- **StintNumber**: The specific number assigned to the stint (e.g., Stint 1, Stint 2).

The **Strategy** column provides an overview of the tyre strategy used by a driver in a given race. For instance, "MEDIUM-MEDIUM-HARD" would indicate that the driver used Medium tyres at the start, switched to Medium again, and ended with Hard tyres.

### 🛞 Inlaps

The [Inlaps.csv](Inlaps.csv) file contains data on **inlaps**, which are the laps when a driver enters the pits. These are identified by a **positive PitInTime** value in the API.

This file shares the same structure and variables as the **DryQuickLaps** dataset. 

### 🛞 Outlaps

The [Outlaps.csv](Outlaps.csv) file contains data on **outlaps**, which are the first lap a driver completes after exiting the pits. These laps are identified by a **positive PitOutTime** value in the API.

This file has the same structure and variables as the **DryQuickLaps** dataset.

### 🛞 Pitstops

The [PitstopsWithTeams.csv](PitstopsWithTeams.csv) file contains detailed data on all the pitstops made during a Grand Prix weekend.

The file includes the following variables:

- **GP**: The name of the Grand Prix (race).
- **Circuit**: The name of the circuit where the race took place.
- **PitstopT**: The total time taken for the pitstop, measured from the moment the driver enters the pit lane until they exit the pit lane.
- **Driver**: The abbreviation of the driver's name (e.g., "VER" for Max Verstappen).
- **Year**: The year of the Grand Prix.
- **Team**: The name of the team the driver was racing for.

This dataset allows for the analysis of pitstop times across various races, circuits, and teams, providing insight into the efficiency of pit crews and race strategies.

### 🚨 Safety Cars

The [SafetyCars.csv](SafetyCars.csv) file contains information on **track status** for each lap of a Grand Prix. This includes the deployment of Safety Cars, Virtual Safety Cars, and other race interruptions.

It includes the following variables:

- **LapNumber**: The lap on which the track status was recorded.  
- **GP**: The name of the Grand Prix.  
- **Year**: The year of the race.  
- **TrackStatus**: A code indicating the status of the track (e.g., Safety Car, Virtual Safety Car).  
- **Label**: A human-readable label for the track status.

The [SafetyCars2024.csv](SafetyCars2024.csv) contains the same type of data, but specifically for the **2024 season**, and is used for model testing purposes.

