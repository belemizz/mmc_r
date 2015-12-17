# Introduction
R scripts for MMC dataset analysis

- Comparison of single time series models
- Comparison of metrics as a input of AR models
- Make animation of prediction and ground truth

# Dataset
Put data files on ../data as "Pt#0X CMU.xlsx" or edit load_data.r before use

# Essencial R libraries
- gdata
- animation
- vars
- forecast

# Scripts
- arima_mmc.r
 - Main script. Run this for executing experimet
- load_data.r
 - Functions for loading data
- plot_data.r
 - Functions for plotting graphs
- predict_data.r
 - Functions for modeling and predicting time series

# Usage
```sh
$ R
> source('arima_mmc.r')
```
- Edit id_number in the script to do experiment for other patients
