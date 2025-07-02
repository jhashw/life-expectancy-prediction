# Predictive Analytics for Life Expectancy Estimation
Predictive models (Linear Regression, Regression Trees, Neural Networks, XGBoost) for life expectancy prediction across 179 countries based on historical socio-economic and health data.

## Abstract
Life expectancy is a crucial metric describing the overall well-being and development of a population. It plays a pivotal role in helping governments understand the effect of public health policies and in allocating funds for retirement programs. In this work, we have explored the relationship between life expectancy and other socio-economic and health metrics of a population such as mortality rates, immunization coverage, and education. We have predicted the life expectancy for a range of countries for the year 2015, given historical data for the years 2000-2014. We found that linear regression is a good model to make predictions for this dataset. However, we improved upon the performance of linear regression by adding data regarding a country’s region in the form of dummy variables. We have also explored the application of other methods such as regression trees, neural networks, and boosting methods such as XGBoost.

## Dataset
The dataset has been obtained from Kaggle ([link](https://www.kaggle.com/datasets/lashagoch/life-expectancy-who-updated/data)) and contains life expectancy, health, mortality, immunization, economic and demographic information about 179 countries between the years 2000 and 2015. The dataset has 20 independent variables, 1 dependent variable (life expectancy) and 2864 rows. The data has been collated from various official sources such as World Bank, World Health Organization (WHO) and the University of Oxford.
Amongst the 20 independent variables, there are two categorical variables – Country and Region. Also, there are two dummy variables to indicate the economic status of a country as ‘Developed’ or ‘Developing’. 

## Workflow
- Exploratory Data Analysis
- Simple Linear regression models
- Linear regression with Region info and variable selection using AIC and BIC
- Regression trees
- Neural nets
- XGBoost




