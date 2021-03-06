# Machine Learning

This repository comprises of several projects completed in IE HST's MS in Business Analytics and Big Data Machine Learning courses.

## Machine Learning I

### 1. Madrid Housing Regression

The main objective of this project was to obtain housing data for sales or rent from a specific region in Spain for a short period of time, and generate a feasible regression model to predict the housing prices. 

We scraped Madrid Housing data using the [Beautiful Soup package](https://pypi.org/project/beautifulsoup4/) from [SpainHouses Website](https://www.spainhouses.net/en/sale-properties-madrid-madrid.html). Extensive data parsing, forming and cleansing was performed.

This resulted in a regression model with features including *Type_of_estate, Num_of_Floors, Not_detached, m2, Plot, Bedrooms, Bathrooms, Parking* and *Garden*. 

`ExploratoryDataAnalysisHousing.R` outlines a brief but complete EDA and `MadridHousingPricesPrediction.ipynb` documents the web scraping, data cleansing, extensive EDA and final modelling which can be viewed [here](https://github.com/maz2198/Machine-Learning/blob/master/ML1/MadridHousingRegression/MadridHousingPricesPredictions(3).ipynb).

### 2. Solar Energy Production Prediction

The main objective of this project was to discover which machine learning methods would provide the best short term predictions of solar energy production. This project is based on a Kaggle Competition: [2014 AMES Solar Competition](https://www.kaggle.com/c/ams-2014-solar-energy-prediction-contest/overview).

We were provided pre-processed data with PCA that we had to model. An extensive EDA, `SOLAR_EDA.R`, was completed and decision on how to model each station was made and can be found [here](https://github.com/maz2198/Machine-Learning/blob/master/ML1/SolarEnergyPrediction/SOLAR_EDA.R). Numerous approaches were taken including a Random Forest Regressor,`SOLAR_RandomForest.R` and the attempt can be found [here](https://github.com/maz2198/Machine-Learning/blob/master/ML1/SolarEnergyPrediction/SOLAR_RandomForest.R). However, the best model was a hyperparamter tuned SVM, `SOLAR_SupportVectorMachines.R` and can be found in [bestSolarModel](https://github.com/maz2198/Machine-Learning/blob/master/ML1/SolarEnergyPrediction/SOLAR_SupportVectorMachines.R).

## Machine Learning II

### 3. HR Turnover

The main purpose of this project is to model the probability of attrition (employees leaving, either on their own or because they got fired) of each individual, as well as to understand which variables are the most important ones and need to be addressed right away using *only* **Logistic Regression**. The two main objectives of this project are to:
- Apply the newly learn data preparation techniques. 
- Highlight the importance of feature engineering in model development.

To be able to build a predictive model on this data, I first start a thorough exploration of the data, I then create a baseline model that I improve on by applying Feature Engineering.

The well documented notebook, `HR_Turnover.ipynb` can be found [here](https://github.com/maz2198/Machine-Learning/blob/master/ML2/HR_Turnover.ipynb) and the dataset is from Kaggle and can be found here: [HR Turnover](https://www.kaggle.com/giripujar/hr-analytics).

### 4. Driven Data: Water Pump Classification

Using data from Taarifa and the Tanzanian Ministry of Water, we were tasked to predict which pumps are functional, which need some repairs, and which don't work at all? Predict one of these three classes (multi-classification) based on a number of variables about what kind of pump is operating, when it was installed, and how it is managed. The reason is a smart understanding of which waterpoints will fail can improve maintenance operations and ensure that clean, portable water is available to communities across Tanzania.

A link to the project, dataset and the leaderboard can be observed [here](https://www.drivendata.org/competitions/7/pump-it-up-data-mining-the-water-table/).

The well documented script, `WaterPumpTanzaniaAnalysis.ipynb` associated with this model can be explored [here](https://github.com/maz2198/Machine-Learning/blob/master/ML2/WaterPumpTanzaniaAnalysis.ipynb). It contains the following:
 - Feature Engineering namely **Frequency Encoding**, **Quantile Binning** and some novel missing imputation techniques 
 - Feature Creation 
 - Modelling using **Decision Tree** with depth optimization, **Bagging**, **Random Forest** with hyperparameter tuning & **Principal Component Analysis**.
 - Comparative analysis of each model produced.

This work was completed with my colleagues Diego Cuartas and Nisrine Ferahi. The developed model finished in the top 15% out of more than 8900 people listed on the Leaderboard with an accuracy score of 0.8147.

