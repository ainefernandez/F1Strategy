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

