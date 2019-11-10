# weather_and_energy_demand

In this project, I fit and assess a model that predicts energy demand based on weather conditions.

This repository contains two R scripts: 
   1. create_tables_dc - A script which downloads, cleans, and joins data on energy demand and weather conditions in Washington, DC, 
      exporting it in the form of a csv file. 
   2. analyze_dc - A script which 
        a) explores and analyzes the data, determines which predictors to use in explaining energy demand, 
        b) settles on a 3rd degree polynomial model, based on analysis of variances and the one standard error rule applied to cross    
           validation error (estimated prediction error),
        c) plots the model against the data points and generates residual plots. 