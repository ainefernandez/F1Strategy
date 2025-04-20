# 🏎️ F1 Race Strategy with GLMs and Decision Trees

Hi there! 👋  

Welcome to the repo for my undergraduate thesis in Data Science — a deep dive into Formula 1 race strategies using Generalized Linear Models (GLMs) and Decision Trees.  

📘 If you speak Spanish, you can check out my full thesis here: [Más allá de la recta: estrategias de Fórmula 1 con modelos lineales generalizados y árboles de decisión](Tesis.pdf)  

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

The [`PitstopsWithTeams.csv`](PitstopsWithTeams.csv) file contains detailed data on all pitstops made during a Grand Prix weekend.

It includes the following variables:

- **GP** – Name of the Grand Prix (race).  
- **Circuit** – Circuit where the race took place.  
- **PitstopT** – Total time of the pitstop, measured from when the driver enters the **pit lane** to when they **exit** it.  
- **Driver** – Abbreviation of the driver's name (e.g., `VER` for Max Verstappen).  
- **Year** – Year in which the Grand Prix occurred.  
- **Team** – Name of the team the driver was racing for.

The [`Pitstops.csv`](Pitstops.csv) file contains the same data **before** the team variable was added.

### 🚨 Safety Cars

The [SafetyCars.csv](SafetyCars.csv) file contains information on **track status** for each lap of a Grand Prix. This includes the deployment of Safety Cars, Virtual Safety Cars, and other race interruptions.

It includes the following variables:

- **LapNumber**: The lap on which the track status was recorded.  
- **GP**: The name of the Grand Prix.  
- **Year**: The year of the race.  
- **TrackStatus**: A code indicating the status of the track (e.g., Safety Car, Virtual Safety Car).  
- **Label**: A human-readable label for the track status.

The [SafetyCars2024.csv](SafetyCars2024.csv) contains the same type of data, but specifically for the **2024 season**, and is used for model testing purposes.

## 🔍 Exploratory Data Analysis

## 📈 Generalized Linear Models (GLMs)

In Formula 1, speed isn’t just about raw pace — it’s about strategy. The faster a driver completes the race distance, the better their finishing position and the more championship points they score. 

Now, imagine you're the only car on track — no overtakes, no traffic, just a clean run to the checkered flag. In that **single car race**, the **optimal race strategy** is the one that minimizes **total race time**. Simple, right?

We can break down that total race time like this:

**Race Time** = **Time on Stints** + **Pitstop Cost**

Each **stint** is a continuous stretch of laps on the same set of tyres. To get the total stint time, we need to estimate the lap time for each of those laps and add them up.

But don’t forget the pitstops! They’re more than just a flashy 2.5-second tyre change — they come with a hidden time penalty:

**Pitstop Cost** = **Inlap LapTime** + **Pitstop Time** + **Outlap LapTime**

I calculate this **Pitstop Cost** for every stop in a strategy. Once I’ve estimated the full stint times and added the pitstop costs, I can compare total estimated race times across different strategies — and find the fastest one. 🏁

And how do we estimate all these times?

With **Generalized Linear Models (GLMs)**, of course! 🔧📉

## 🤔 What are Generalized Linear Models (GLMs)?

If you’re already familiar with classic linear models, then GLMs are like their more flexible, modern cousin. They serve the same basic purpose: to model the relationship between a **dependent variable (Y)** and one or more **explanatory variables (X)**.

The major advantage of GLMs over traditional linear models? **They don’t require the dependent variable to follow a normal distribution.** Instead, GLMs allow the response variable to follow any distribution from the **exponential family** (like Poisson, Gamma, Binomial, etc.), making them a much more versatile tool in real-world data analysis.

GLMs also relax some of the classic assumptions — like linearity, normality, and homoscedasticity — making them ideal for situations where those assumptions don't hold.

### GLMs have two main components:

- **Error Distribution (Family):**  
  This is the distribution of the dependent variable (Y). It must belong to the exponential family — which includes Normal, Poisson, Gamma, and others — chosen based on the nature of the data.

- **Link Function (g()):**  
  This function connects the **mean of the dependent variable** to the **linear predictor** (the linear combination of the independent variables). It must be **monotonic** and **differentiable**, which ensures a unique relationship between the predictor and the expected value of Y.

### The general form of a GLM

`Y =  g⁻¹(Xβ) + ε`

Where:  
- `g⁻¹` is the inverse of the link function  
- `Xβ` is the linear predictor  
- `ε` is the error term

### Model Comparison

To compare different GLMs, we rely on model selection criteria like:

- **Akaike Information Criterion (AIC):**

  `AIC = -2 * log(L) + 2k`

- **Bayesian Information Criterion (BIC):**

  `BIC = -2 * log(L) + k * log(n)`

Where:  
- `L` is the likelihood of the model  
- `k` is the number of estimated parameters  
- `n` is the sample size

While **AIC** emphasizes *predictive performance*, **BIC** favors models that strike a balance between *simplicity and fit*. In this thesis, I focus  more on **AIC** for model selection.

## 🧠 Model Selection

After diving into the data through Exploratory Data Analysis, I had a pretty good idea 💡 of which variables could impact lap times and pit stop costs. To lock in the best model, I used the `bestglm` function from the **bestglm** R package — a powerful tool that compares models using the **Bayesian Information Criterion (BIC)**.

But first, let’s talk families. Since I'm modeling **continuous** and **strictly positive** variables (lap times and pit stop costs), I explored three distribution families:

- **Normal** (with a logarithmic link function)  
- **Gamma** (with an inverse link function)  
- **Inverse Gaussian** (with a quadratic inverse link function)

Once I had the top model from each family, I brought out the **Akaike Information Criterion (AIC)** — because **lower AIC = better model** 📉. This way, I picked the one that has the best predictive performance.

### 🏁 LapTimePerKM: How Fast Can You Go?

To estimate how lap times evolve across a stint, I modeled the variable `LapTimePerKM`. The champion here? 🥇 The model using the **Inverse Gaussian** family.

Here’s how the model looks:

**LapTimePerKM** = *g⁻¹*(β₀ + β₁·Circuit + β₂·RacePercentage + β₃·Driver + β₄·Team + β₅·TyreLife + β₆·Compound + β₇·Position + β₈·Stint) + ε

This means lap time per kilometer depends on:

- 🏎️ **Circuit** – because every track is unique  
- 📈 **RacePercentage** – lap times change as the race progresses  
- 🧑‍💼 **Driver** & **Team** – skill and performance matter  
- 🛞 **TyreLife** & **Compound** – fresh softs ≠ worn hards  
- 🏁 **Position** – cleaner air vs battling in traffic  
- 🔁 **Stint number** – drivers push differently across stints

### 🛞 Pitstop Time 

Pit stops might look quick on TV, but they're complex beasts in data! To estimate how long a driver actually spends going through the pit lane — from entry to exit — I modeled the variable `PitstopT`.

And the winner is... 🥇 **Inverse Gaussian** family.

The model is nice and simple:

**PitstopT** = *g⁻¹*(β₀ + β₁·Circuit) + ε

In other words, the only factor that significantly impacts pit stop time is the **Circuit** itself — which makes sense! Some pit lanes are longer or slower. 

### 🛞 Inlaps

Inlaps — the lap where a driver dives into the pits — are often overlooked, but they're crucial for calculating the full cost of a pit stop. I modeled them using the variable `LapTimePerKM` (standardized lap time), focusing specifically on inlap data.

🏁 **And the winner is...** once again, the **Inverse Gaussian** family 🥇

The best-fitting model looks like this:

**LapTimePerKM** = *g⁻¹*(β₀ + β₁·Circuit + β₂·Compound + β₃·TyreLife + β₄·Stint) + ε

This tells us that the **circuit**, **tyre compound**, **age of the tyres**, and **stint number** all play key roles in determining how quick (or slow) that final inlap is before a pit stop. It's like the last gasp of a tyre's life — and I want to time it just right 🔧⏱️.

### 🛞 Outlaps

Outlaps — the lap right after a pit stop — are when drivers rejoin the track on fresh rubber, but not necessarily at full speed yet. Cold tyres, and traffic can all affect performance here.

🏁 **And the winner is...** once again, the **Inverse Gaussian** family! 🥇

The best model to estimate `LapTimePerKM` for outlaps is:

**LapTimePerKM** = *g⁻¹*(β₀ + β₁·Circuit + β₂·Compound) + ε

This tells us that outlap pace is mostly driven by the **circuit characteristics** and the **type of tyre compound** the driver switches to.

### 🚨 Safety Cars

Can we predict when a Safety Car will appear during a race? 🧐 To find out, two logistic GLMs models (binomial family with a **logit** link function) were compared:

- 🛣️ **Model 1:** Includes only the lap number (`LapNumber`).
- 🏁 **Model 2:** Adds the effect of the circuit (`Circuit`), because not all tracks are equally chaotic...

The final model looks like this:

**SafetyCar** = *g⁻¹*(β₀ + β₁·LapNumber + β₂·Circuit) + ε

This allows us to estimate the probability of a Safety Car being deployed based on the lap number and the circuit. After all, Jeddah is not Monza... 😉

### 🧮 Estimation

The [R script](2.ModelSelection.R) handles the heavy lifting for estimation and model selection 🧠📊

You can check out the full results in [Chapter 4](Tesis.pdf) of my thesis *“El plan perfecto para la victoria: Modelos”* — specifically, Tables 4.2 to 4.17.


## 🌳 Choosing the Winning Strategy: Decision Trees

Now that we can estimate **stint times** and **pitstop costs**, it’s time to make some strategic calls — enter the **decision trees**! 🧠🛞

A decision tree is like a roadmap of choices. It starts with a root node (your first big decision) and branches out into all the possible paths your race strategy can take.

In our case, each **final node** of the tree shows the **total expected race time** of a strategy — calculated using the GLMs from the previous sectin laptimes, inlaps, outlaps, and pitstop times.

Here's how it works:

1. **Root Node**: Choose your starting tyre — **Soft (S)**, **Medium (M)**, or **Hard (H)**.
2. **First Pitstop**: Pick your next tyre set (mandatory pitstop = mandatory decision 🚨).
3. **Second Pitstop**: 
   - If you’ve repeated a compound, you **must** pit again.
   - Otherwise, you can pick another compound or **not pit (NP)** and go to the end!

At the end of each path, we get an estimated **Race Time**. The strategy with the **lowest time** wins the simulation — and maybe the race too. 🏁🚀

All the decision trees models can be found in this [R script](3.DecisionTreeModels.R).

### 🔧 Deterministic Model

This model simulates a **single car race**, where pitstops are only allowed on the lap of the expected tyre life of the compound, making it a fully **deterministic setup**. It explores all possible strategies using the decision tree from earlier. 🧠🌳

The simulation runs in **three rounds**, each starting on a different dry compound:

1. **Round 1**: Start on **Hard** tyres (H)  
2. **Round 2**: Start on **Medium** tyres (M)  
3. **Round 3**: Start on **Soft** tyres (S)

**How it works:**
Using the GLMs models from the previous section: 

- First, the model calculates the **stint duration** for the starting compound compound.
- Then it adds the **pitstop cost**, defined as:  
  `pitstop cost = inlap + pit time + outlap`
- It checks if the compound lasts long enough, follows tyre rules (you must use at least two different compounds), and whether a second or third stop is required.

Each path through the tree returns:
- Total race time (sum of all stints + pitstop costs),
- Any warnings (like tyre life being exceeded),
- And the **best strategy**, based on the shortest total time. 🏁

💡 In races where pitstops are expensive or tyres are long-lasting, the model also considers simpler **two-stint strategies** — leaner, but still fast.


### ⏳ Window Model

Race strategists aren’t just interested in *which* tyres to use — they also care about *when* to pit. What happens if a driver pits two laps earlier than planned? Or a few laps later? That’s where the **pit window** comes in. 🪟

A **pit window** is the range of laps around the *ideal* pit stop where a driver might realistically come in — usually a few laps before or after the planned stop.

This model builds on the same decision tree as the deterministic model, **but with one big difference**:  
Instead of forcing the first stop to happen exactly when the tyres reach their expected life, the model allows for flexibility — you can pit a few laps earlier or later.

We test this using a range of **window values**:  
`{-3, -2, -1, 0, 1, 2, 3}`

- A value of `-3` means pitting **three laps earlier** than planned  
- A value of `0` is the expected tyre life  
- A value of `3` means pitting **three laps later**

For each window value, the model calculates:
- Total race time 
- All valid strategy paths 
- The fastest option 

The Window Model gives teams the **flexibility** to adapt during the race — whether it’s reacting to an on-track incident, responding to a rival’s strategy, or adjusting for changing weather.

### 🥊 Direct Rival Model

Up to now, the models I've described are static — they calculate the *best* strategy assuming a fixed position after the first lap, without accounting for what’s happening on track. But in a real race, things are constantly changing.

That’s where the **Direct Rival Model** comes in. It’s my first approach to **dynamic strategy**, where timing decisions depend on the **gap to your closest rival**.

The model recalculates strategies *every lap* based on:
- The time gap to your nearest competitor ⏱️  
- The potential to gain track position through an **undercut** (pit earlier) or **overcut** (pit later)

It tests three pit stop timings relative to tyre life:
1. Pit 3 laps earlier
2. Pit on the expected lap
3. Pit 3 laps later

Then it estimates how much time you'd gain or lose compared to your rival with each option.

#### Limitations? Yep.

It’s a big step toward real-time strategy modeling, but it’s not fully dynamic just yet. Why?
- We’d need **exact live time gaps** to model lap-by-lap decisions perfectly.
- Things like safety cars, pit lane traffic, or unexpected incidents can throw off predictions based solely on average lap times.

Still, it’s a solid starting point for **thinking strategically — not just in isolation, but against the competition.**

## 🏁 Results

To evaluate the accuracy of the models, I tested them using data from four races in the 2024 season. Three of these — **Bahrain**, **Singapore**, and **Monza** — were run without interruptions from Safety Cars or Virtual Safety Cars (VSC). The fourth, **China**, experienced multiple Safety Car interventions, offering a contrasting scenario.

The tyre compound selected for the race start can vary due to several factors — including each team's tyre allocation and specific race-day strategies. For this reason, the analysis considers the **fastest strategy** for each type of starting compound:

- One for drivers who began on **Soft** tyres
- One for **Medium**
- And one for **Hard**

It’s crucial to emphasize that real-world strategies don’t always align with what the model identifies as optimal. While the comparison between real and predicted strategies offers a way to assess the model's accuracy, we must recognize that teams often make **suboptimal choices** — and not necessarily by mistake.

Factors such as:
- Tyre allocation constraints 
- Strategy miscalculations 
- Tactical risks to chase more points or go for a win   
can lead teams to diverge from the theoretically optimal plan.

Thus, when reviewing the results in the upcoming examples, keep in mind: the strategy a team used may not have been the best possible — but it was chosen within the context of that specific race.

### 🇧🇭 2024 Bahrain GP

These are the strategies used by the drivers in the 2024 Bahrain Grand Prix:

<div align="center">
  <img width="500" alt="2024 Bahrain GP Strategies" src="https://github.com/user-attachments/assets/ef2fef3c-e752-4e29-a7e1-5d7b8bc3b4eb" />
</div>

🔴 **Soft** | ⚪ (gray) **Hard** | 🟡 **Medium**

In this race, only **Soft** and **Hard** compounds were used. The Bahrain GP consists of **57 laps**, so any bar reaching lap 57 represents a full race distance. Drivers like Zhou (ZHO), Magnussen (MAG), Ricciardo (RIC), Tsunoda (TSU), Albon (ALB), Hulkenberg (HUL), Ocon (OCO), Gasly (GAS), and Bottas (BOT) were **lapped once**, and Sargeant (SAR) finished **two laps down**.

#### 🔧 Deterministic Model

Although expected lap times vary across teams and drivers, the **optimal strategy** identified by the model was the same for all in this case:  
**Soft–Hard–Hard**.

Since all drivers started on **Soft** tyres, only this initial compound is considered for strategy accuracy. According to the model, **14 out of 20 drivers** followed the optimal strategy, resulting in a **70% precision rate**.

This strategy was also the **most popular** among the grid. However, drivers like **Max Verstappen** and **Sergio Pérez** (Red Bull Racing) opted for a different approach. This divergence can be explained by tyre allocation constraints:

- Both had:  
  - 1 set of **new Softs**  
  - 3 sets of **used Softs**  
  - Only 1 set of **new Hards**

Meanwhile, other Top 10 drivers had **two sets of new Hard tyres**, giving them more flexibility to follow the model's optimal plan. This example highlights how tyre allocation can play a decisive role in strategic decisions.

#### 🪟 Window Model

According to the **Window Model**, the best strategy involved stopping **3 laps earlier** than the expected tyre lifespan.  
The optimal tyre sequence proposed by this model was:  
**Medium–Medium–Hard**

However, in this specific race, no driver started on **Medium** tyres, so the strategy was not applicable. This demonstrates that even when a model suggests a faster plan, it may not always be implementable due to constraints in tyre availability or allocation.

### 🇸🇬 2024 Singapore GP

These are the strategies used by the drivers in the 2024 Singapore Grand Prix:

<div align="center">
  <img width="500" alt="2024 Singapore GP Strategies" src="https://github.com/user-attachments/assets/2f8f83ba-746c-4dae-b0be-8dd5be9ba114" />
</div>

🔴 **Soft** | ⚪ (gray) **Hard** | 🟡 **Medium**

In this race, **Soft** and **Hard** compounds were used. The Singapore GP consists of **62 laps**, so any bar reaching lap 62 represents a full race distance. Drivers from positions 8 to 18 finished **one lap down**, while **Albon (ALB)** retired on lap 15, and **Magnussen (MAG)** retired on lap 57.

#### 🔧 Deterministic Model

The **optimal strategy** identified by the model was the same for everyone:  
**Medium–Hard–Hard**.

Out of the 20 strategies used by drivers, **15 matched the optimal strategy**, giving us a **75% precision rate**. The **Medium-Hard** strategy was the most common, and some drivers also chose to start with **Hard** tyres.

##### Accuracy by Initial Compound:
- **Hard**: 15% precision (Hard-Medium strategy)
- **Medium**: 60% precision (Medium-Hard strategy)
- **Soft**: 0% precision (Soft-Hard-Hard strategy)

Interestingly, Mercedes made a bold move with Lewis Hamilton, who started on Softs in 3rd position. The strategy aimed to take advantage of a potential Safety Car period, which was a reasonable gamble considering that a Safety Car had occurred 100% of the time** in previous years at Singapore. Unfortunately, the Safety Car never came, and this gamble cost Hamilton 3 positions by the end of the race.

#### 🪟 Window Model

The **Window Model** suggested a strategy that involved stopping **3 laps earlier** than the expected tyre life, recommending the sequence:  
**Soft–Hard–Hard**.

### 🇮🇹 2024 Monza GP

These are the strategies used by the drivers in the 2024 Monza Grand Prix:

<div align="center">
  <img width="500" alt="2024 Monza GP Strategies" src="https://github.com/user-attachments/assets/97e4c967-39b7-4c74-b9e1-12121f92f079" />
</div>

🔴 **Soft** | ⚪ (gray) **Hard** | 🟡 **Medium**

The **Monza GP** consists of 53 laps. Drivers from P1 to P13 completed the full race distance, while the rest finished **one lap down**. The only retirement of the race was **Yuki Tsunoda (TSU)**, who pulled out on lap 7.

#### 🔧 Deterministic Model

Monza is known as the "Temple of Speed" but in 2024, it also became a playground for strategy games. 

According to the deterministic model:

- **9 out of 20** real-life strategies matched the model’s optimal picks.
  - **7** drivers used the recommended **Medium–Hard** strategy.
  - **2** drivers used the **Hard–Medium** variant.
- This results in a **45% overall precision rate** — lower than other races, likely due to the split between **one-stop** and **two-stop** strategies.

##### Accuracy by Initial Compound:
- **Hard**: 10% (Hard–Medium)
- **Medium**: 35% (Medium–Hard)
- **Soft**: 0% (Soft–Hard–Hard)
- **Overall**: **45% precision**

🏁 **Charles Leclerc** thrilled the Tifosi by winning the race after starting P3, executing the optimal Medium–Hard strategy to perfection — textbook Ferrari at home!

On the flip side, **McLaren** rolled the dice with **Lando Norris** and **Oscar Piastri**, who locked out the front row but went for a **two-stop strategy**. While bold, it might’ve cost them the win in a race where less was more.

#### 🪟 Window Model

The **Window Model**, which advises pitting **3 laps earlier** than the expected tire lifespan, proposed a slightly different take:

- Optimal strategy: **Medium–Medium–Hard**

### 🇨🇳 2024 China GP

These are the strategies used by the drivers in the 2024 China Grand Prix:

<div align="center">
  <img width="500" alt="2024 China GP Strategies" src="https://github.com/user-attachments/assets/d640ea4b-a131-4970-981f-8cef6ff0d636" />
</div>

🔴 **Soft** | ⚪ (gray) **Hard** | 🟡 **Medium**

The **Chinese Grand Prix** consists of 56 laps. All drivers completed the race distance except for **Ricciardo (RIC)**, **Tsunoda (TSU)**, and **Bottas (BOT)**, who retired on laps 33, 27, and 20, respectively.

---

#### 🔧 Deterministic Model

According to the model:

- Only **4 out of 20** real-life strategies matched the optimal predictions, all using **Medium** as the initial compound.
- This results in a **20% precision rate**, reflecting the challenge of forecasting under unpredictable conditions.

##### Accuracy by Initial Compound:
- **Hard**: 0% (Hard–Medium)
- **Medium**: 20% (Medium–Hard)
- **Soft**: 0% (Soft–Hard–Hard)
- **Overall**: **20% precision**

Although **Medium** was the most popular starting compound, a few drivers went with **Hard** or **Soft**. However, the race took an unexpected turn due to **external interventions**.

This GP was specifically chosen to test the model’s robustness under chaotic conditions. The **2024 Chinese GP featured two Safety Car periods** that drastically influenced race strategies:

🟡 **Example**:  
**Verstappen** and **Pérez** (Red Bull Racing) had just pitted for fresh tyres before the first Safety Car came out. Despite this, both pitted again under the Safety Car, showing how rapidly plans can change.  
A second Safety Car followed shortly after the restart, reshuffling strategies further.

These interventions allowed some drivers to turn their race into a one-stop, while others adapted with multiple stops. As a result, the model’s predicted strategies were less frequently followed — not due to inaccuracy, but because reality deviated from the plan in ways the model could not foresee. The model does not account for the occurrence of Safety Cars, which significantly alter tyre strategy and race dynamics.

#### 🪟 Window Model

The **Window Model**, which recommends stopping **3 laps earlier** than the estimated tyre lifespan, suggested an **aggressive strategy** for this race:

- Optimal sequence: **Soft–Hard–Hard**

However, given the **disruptions from two Safety Cars**, many drivers couldn’t stick to a fixed stint plan. This highlights the limitations of both models in unpredictable race conditions — and the need for adaptability in real-time strategy calls.

### 🚨 Safety Cars

To test the Safety Car model, all circuits in the 2024 calendar (up to Singapore) were classified into three groups according to risk:

| Percentage Range (%) | Risk Category |
|----------------------|----------------|
| 0 - 30               | Low Risk       |
| 30 - 70              | Medium Risk    |
| 70 - 100             | High Risk      |

#### Estimated Probabilities by Risk Category

- The model was run for each risk group.
- Safety Car probabilities were calculated based on the percentage of the race completed.

**Low Risk, Medium Risk, High Risk**  
<img width="500" alt="Screen Shot 2025-04-19 at 21 19 07" src="https://github.com/user-attachments/assets/d88afc4b-58c0-4837-a3d7-df1492193416" />  
<img width="500" alt="Screen Shot 2025-04-19 at 21 19 37" src="https://github.com/user-attachments/assets/1301fc72-710c-42c9-adf6-4dad254c4ef1" />  
<img width="500" alt="Screen Shot 2025-04-19 at 21 20 26" src="https://github.com/user-attachments/assets/5057e591-f6c1-420e-bf31-d09b41d1c82e" />

##### Key Observations

- **Low-Risk Circuits** (e.g., China and Hungary):
  - Hungary shows consistently low probabilities throughout the race.
  - China, despite limited data (only raced once before in 2019), exhibits a downward trend toward the end of the race.

- **Medium-Risk Circuits**:
  - Display a similar pattern to low-risk tracks.
  - Peak probability at the beginning of the race, gradually decreasing.
  - Overall higher probabilities than low-risk tracks.

- **High-Risk Circuits**:
  - Show elevated Safety Car probabilities throughout the entire race.
  - These tracks are the most volatile, especially in early laps.

#### Average Probability by Risk Group

<img width="500" alt="Screen Shot 2025-04-19 at 21 20 53" src="https://github.com/user-attachments/assets/b55c7e35-8fd7-46ef-b2f3-0bebc4dbafca" />

- High-risk circuits have the **highest average probability** of a Safety Car.
- Followed by medium-risk, and finally low-risk circuits.
- All categories show a **maximum probability early in the race**, which **declines as the race progresses**.

## 🧠 Debrief

This thesis set out to design a race strategy model to help Formula 1 teams make smarter decisions and find the optimal strategy for each Grand Prix. Using data from F1’s official API, I began by exploring key factors that influence race strategy — think tire wear, pit stop times, race incidents — and built models around them.

### 🛠️ Building the Model

- **Started simple:** Used multiple linear regressions to estimate lap times and pit stop losses.
- **Faced reality:** Despite good R² scores, assumptions didn’t hold — so I moved to **generalized linear models** (GLMs), which handled things better.
- **Decision making:** Strategy isn’t just prediction — it’s also choice. So I added **decision trees** to help pick between options.

### 🏁 Model Highlights

- **Deterministic Model:** 
  - Worked great in clean races like **Bahrain 2024**.
  - Less accurate in chaotic races (e.g., **China 2024** with multiple Safety Cars).
  - Struggled when teams tried creative pit stop strategies — looking at you, **Monza 2024**.

- **Real Strategy Comparison:**  
  - Model results were compared with actual team strategies — though, let’s be honest, even teams make strategic errors (**Singapore 2024**, anyone?).

- **Window Model:**  
  - Suggested undercutting about **3 laps before** tire life expires — a pattern that aligns with real-world decisions when drivers are stuck in DRS trains or struggling with worn tires.

### 🔮 What’s Next?

This is just the starting line. Future models could:

- Factor in **Safety Car probabilities**.
- Adapt to **wet track conditions** for better tire choices.
- Get more **dynamic**, updating strategies when drivers suddenly gain or lose positions.

One early attempt at this is the **Rival Model**, which starts considering time gaps between drivers — a small but exciting step toward real-time adaptability.

### 🧩 Final Thoughts

This work adds a new voice to the race strategy literature — using GLMs and decision trees instead of neural nets or dynamic programming. It's a flexible, interpretable approach that lays a solid foundation for future, more reactive models.

Ultimately, this thesis shows the **potential of data-driven strategy models** to support F1 teams in a world where every tenth of a second counts. With added layers like Safety Car risk and live updates, the future looks fast — and smart.

## 📚 References


- Collins, B. (2024). *How to Win a Grand Prix: From Pit Lane to Podium - The Inside Track*. Quercus.
- Dunn, P. K., Smyth, G. K., et al. (2018). *Generalized linear models with examples in R, volume 53*. Springer.
- Formula 1 (2024a). F1 live timing. [https://www.formula1.com/en/timing/f1-live](https://www.formula1.com/en/timing/f1-live).
- Formula 1 (2024b). It Ruined His Race: Mercedes Admit to Clear Mistake with Hamilton’s Singapore GP Strategy. [https://www.formula1.com/en/latest/article/it-ruined-his-race-mercedes-admit-to-clear-mistake-with-hamiltons-singapore.4VteePMCYiSgKE7NAq9TY6](https://www.formula1.com/en/latest/article/it-ruined-his-race-mercedes-admit-to-clear-mistake-with-hamiltons-singapore.4VteePMCYiSgKE7NAq9TY6).
- Formula 1 (2024c). Leclerc Thrills the Tifosi to Triumph at Monza Ahead of Piastri and Norris with Bold Ferrari Strategy Paying Off. [https://www.formula1.com/en/latest/article/leclerc-thrills-the-tifosi-to-triumph-at-monza-ahead-of-piastri-and-norris.1aiYZF3rWZp2Q9yQtcuvqV](https://www.formula1.com/en/latest/article/leclerc-thrills-the-tifosi-to-triumph-at-monza-ahead-of-piastri-and-norris.1aiYZF3rWZp2Q9yQtcuvqV).
- Formula 1 (2024d). Strategy Guide: What Are the Possible Race Strategies for the 2024 Bahrain Grand Prix. [https://www.formula1.com/en/latest/article/strategy-guide-what-are-the-possible-race-strategies-for-the-2024-bahrain.1NT25ROUaq4grnLyJmOojV](https://www.formula1.com/en/latest/article/strategy-guide-what-are-the-possible-race-strategies-for-the-2024-bahrain.1NT25ROUaq4grnLyJmOojV).
- Formula 1 (2024e). Verstappen charges to victory over Norris and Perez in action-packed Chinese GP. [https://www.formula1.com/en/latest/article/verstappen-charges-to-victory-over-norris-and-perez-in-action-packed-chinese.3Uz5CwNh5tEQt62umIGhob](https://www.formula1.com/en/latest/article/verstappen-charges-to-victory-over-norris-and-perez-in-action-packed-chinese.3Uz5CwNh5tEQt62umIGhob).
- Heilmeier, A., Thomaser, A., Graf, M., and Betz, J. (2020). Virtual strategy engineer: Using artificial neural networks for making race strategy decisions in circuit motorsport. *Applied Sciences*, 10(21):7805.
- Magee, J. F. (1964). Decision trees for decision making. *Harvard Business Review*, Brighton, MA, USA.
- McCarthy, L. and Rotthoff, K. W. (2013). Incentives on the starting grid in formula one racing. *The Journal of Sport*, 2(2).
- Schaefer, P. (2024). FastF1. [https://docs.fastf1.dev/](https://docs.fastf1.dev/).
- Stoppels, E. (2017). Predicting race results using artificial neural networks. Master’s thesis, University of Twente.
- Thraves, C. et al. (2022). On the optimization of pit stop strategies via dynamic programming. *Central European Journal of Operations Research*, 31(1).
