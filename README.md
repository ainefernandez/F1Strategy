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

Next, I explain which variables are contained in each of these datasets

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

The [DryQuickLaps.csv](DryQuickLaps.csv) file contains data from laps driven in **dry conditions** only. Races affected by rain were excluded to maintain consistency in tyre performance and strategy evaluation. Additionally, only **competitive laps** were kept, filtered according to Formula 1’s 107% rule — meaning each lap had to be within 107% of the fastest lap.

This dataset includes the following variables:

- **Driver** – Name of the driver who set the lap  
- **Team** – Team the driver was racing for  
- **LapNumber** – Number of the lap in which the time was set  
- **LapTime** – Time the driver took to complete the lap  
- **Stint** – The stint number (i.e., which tyre set this lap belongs to)  
- **Compound** – Tyre compound used (Soft, Medium, Hard)  
- **TyreLife** – Number of laps the tyre set had completed when the lap was recorded  
- **Position** – Driver's track position at the moment of the lap  
- **Year** – Season in which the lap was recorded  
- **GP** – Grand Prix in which the lap was recorded  
- **Length** – Track length in kilometers  
- **Abrasion** – Level of track abrasion  
- **Traction** – Level of traction required by the circuit  
- **Braking** – Intensity of braking on the circuit  
- **TrackEvo** – Level of track evolution (how much grip improves over the weekend)  
- **Grip** – General grip level of the track  
- **Lateral** – Amount of lateral force experienced (high in circuits with many corners)  
- **Downforce** – Aerodynamic downforce needed at the circuit  
- **TyreStress** – Overall stress the track puts on the tyres  
- **LapTimePerKM** – Standardized lap time (lap time divided by circuit length)  
- **Laps** – Total number of laps in the race  
- **RacePercentage** – Percentage of the race distance completed when the lap was recorded

### 🔁 Stints

The [Stints.csv](Stints.csv) file contains data on **stints**, which refer to the continuous number of laps a driver completes on the same set of tyres before making a pit stop.

It includes:

- **Driver**: Name of the driver  
- **Stint**: Stint number (e.g., 1st, 2nd, etc.) during the race  
- **Compound**: Tyre compound used in the stint (SOFT, MEDIUM, HARD)  
- **GP**: Grand Prix name  
- **Year**: Year of the event  
- **StintLength**: Number of laps in the stint

### 🛞 Inlaps

The [Inlaps.csv](Inlaps.csv) file contains data on **inlaps**, which are the laps when a driver enters the pits. These are identified by a **positive PitInTime** value in the dataset.

This file shares the same structure and variables as the DryQuickLaps.csv dataset. 

### Outlaps
