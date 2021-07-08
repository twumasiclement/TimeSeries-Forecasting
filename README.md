# TimeSeries-Forecasting (External R scripts and data for a paper submitted to the International Journal of Forecasting).

Topic: Machine Learning Algorithms for Forecasting and Backcasting Blood Demand Data with Missing Values and Outliers: A Study of Tema General Hospital of Ghana.

The 6 machine learning (ML) methods used were:

K-Nearest Neighbor regression (KNN)

Neural Network Auto-Regressive (NNAR)

Generalized Regression Neural Network (GRNN)

Multi-Layer Perceptron (MLP) neural networks

Extreme learning machines (ELM) neural networks

Long short-term memory (LSTM) neural network.

NB: These novel R functions (attached) were respectively developed to implement the six ML time-series models and the non-seasonal ARIMA model via a rolling-origin strategy for model comparison (for both forecast and backcast schemes).

The key contribution of the study was:
1. To demonstrate the application of some ML algorithms for also backcasting or predicting lost blood demand data of past years.
2. To establish a baseline comparative study for the predictive performance of ML models for short time-series data with missing values and outliers.
3. To justify the need for an out-of-sample rolling-origin strategy in comparing existing time-series models' forecasting and backcasting power for short time-series data.
