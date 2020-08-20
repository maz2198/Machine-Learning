# Machine Learning

This repository comprises of several projects completed in IE HST's MS in Business Analytics and Big Data Machine Learning courses.

## Machine Learning I

### 1. Madrid Housing Regression

### 2. Solar Energy Prediction

## Machine Learning II

### 3. HR Turnover

### 4. Driven Data: Water Pump Classification

Using data from Taarifa and the Tanzanian Ministry of Water, we were tasked to predict which pumps are functional, which need some repairs, and which don't work at all? Predict one of these three classes (multi-classification) based on a number of variables about what kind of pump is operating, when it was installed, and how it is managed. The reason is a smart understanding of which waterpoints will fail can improve maintenance operations and ensure that clean, portable water is available to communities across Tanzania.

A link to the project, dataset and the leaderboard can be observed [here](https://www.drivendata.org/competitions/7/pump-it-up-data-mining-the-water-table/).

The well documented script, `WaterPumpTanzaniaAnalysis.ipynb` associated with this model can be explored [here](https://github.com/maz2198/Machine-Learning/blob/master/ML2/WaterPumpTanzaniaAnalysis.ipynb). It contains the following:
 - Feature Engineering namely **Frequency Encoding**, **Quantile Binning** and some novel missing imputation techniques 
 - Feature Creation 
 - Modelling using **Decision Tree** with depth optimization, **Bagging**, **Random Forest** with hyperparameter tuning & **Principal Component Analysis**.
 - Comparative analysis of each model produced.

This work was completed with my colleagues Diego Cuartas and Nisrine Ferahi. The developed model we finished in the top 15% out of more than 8900 people listed on the Leaderboard with an accuracy score of 0.8147.

