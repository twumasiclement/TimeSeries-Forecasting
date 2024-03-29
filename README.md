# Time-series prediction for blood demand (External R scripts and data for a paper submitted to the International Journal of Forecasting).

Topic: Machine Learning Algorithms for Forecasting and Backcasting Blood Demand Data with Missing Values and Outliers: A Study of Tema General Hospital of Ghana.

The 6 machine learning (ML) methods used were:

K-Nearest Neighbor regression (KNN)

Neural Network Auto-Regressive (NNAR)

Generalized Regression Neural Network (GRNN)

Multi-Layer Perceptron (MLP) neural networks

Extreme learning machines (ELM) neural networks

Long short-term memory (LSTM) neural network.

# NB: 
The non-seasonal ARIMA model was considered as a baseline model for comparison with the ML models. These novel R functions (attached) were respectively developed to implement the six ML time-series models and the non-seasonal ARIMA model via a rolling-origin strategy for model comparison (for both forecast and backcast schemes). The main R script for the study is named `Main_Script_IJF.r` or `Main_Script_IJF.html` (i.e .R  and .html file formats, respectively); and the other R scripts were external files sourced in the main script. The R codes were created with the help of other packages (properly refrenced in the main paper) and open-source R libraries Keras and TensorFlow. The empirical data on blood demand with missing values from Jan 2013 to Sept 2020 (which was used for model fitting) is also attached as `Blood_data_Tema.csv`.

The external scripts (for implementing the rolling-origin forecast and backcast schemes) sourced in the main script `Main_Script_IJF.r` are: `rolling-eval-ARIMA-script` (ARIMA); `ro-eval-KNN-script.R` (KNN); `ro-eval-NNAR-script.R`(NNAR); `ro-eval-GRNN-script.R` (GRNN); `ro-eval-MLP-script.R` (MLP); `ro-eval-ELM-script.R` (ELM); `rolling-eval-LSTM-script.R`(LSTM). 

# Research gap and Contribution:

Forecasters, policymakers, time-series users and practitioners usually need long time-series data for model assessment, policy analysis, and investigating underlying trends and patterns to aid decision-making. Nevertheless, such long time-series data may not always be obtainable at the expected frequency, with the needed temporal or spatial coverage of the data unavailable in previous years, especially among blood centres in developing countries. The data unavailability may be due to several reasons such as human error, human failure, software corruption, data storage destruction, lack of the required data processors, or data collection only started in some future time, amongst others. Consequently, it is imperative to estimate or predict the lost data of past years for time-series users. The process of predicting data of past years is referred to as backcasting or reverse forecasting. It is possible to backcast or forecast in reverse time for relevant time-series provided the series is strictly stationary and time-reversible. Unfortunately, to the best of our knowledge, no known study has explored the backcasting power of the existing state-of-the-art ML algorithms for predicting unavailable blood demand data of past years. Hence, this current study attempts to bridge this gap by investigating the forecasting and backcasting power of a few selected ML algorithms (KNN, NNAR, GRNN, MLP, ELM and LSTM) for a short time-series data on blood demand with past lost values using an out-of-sample rolling-origin evaluation strategy for model comparison. The current study can further be expanded by time-series modellers for other general modelling problems with short series contaminated with missing values and outliers, and the findings can help policymakers in the management of the blood supply chain (as previously discussed) as well as provide publicly accessible adaptive programming codes in R (with the help of existing packages) to forecast or backcast any time-series data using the underlying time-series models via the proposed rolling-origin strategy. 


# The key contribution of the study was:

1. To demonstrate the application of ML algorithms and a classical time-series model for also backcasting lost data of past years or any time-reversible stationary series.
2. To establish that the direction of prediction (forecasting or backcasting) can affect the predictive performance of ML models given time-series data.
3. To justify the need for an out-of-sample rolling-origin strategy in comparing existing time-series models' forecasting and backcasting power for short time-series data (with missing values and outliers).

