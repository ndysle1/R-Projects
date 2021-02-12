## Stock Estimation Overview
#### This project began with 5 datasets, each containing 200+ financial indicators that are commonly found in the 10-K filings of publicly traded companies.

#### The original datasets were very messy, containing a large number of missing values and some outliers.

#### The most important columns in the datasets are:
 - PRICE VAR [%]: This variable lists the percent price variation of each stock for the year. 
 - Class: This variable corresponds to the PRICE VAR [%] and is either 0 or 1.
 
#### Examples:
  - If the PRICE VAR [%] value is positive, class = 1. From a trading perspective, the 1 identifies those stocks that an hypothetical trader should BUY at the start of the year and sell at the end of the year for a profit.
  - If the PRICE VAR [%] value is negative, class = 0. From a trading perspective, the 0 identifies those stocks that an hypothetical trader should NOT BUY, since their value will decrease, meaning a loss of capital.
  
#### The inclusion of both these variables make it possible to create both classification and regression models on the datasets.

#### About the Data:
 - The financial CSVs came from a [Kaggle Project](https://www.kaggle.com/cnic92/200-financial-indicators-of-us-stocks-20142018) and, according to the creator, information was scraped from Financial Modeling Prep API.

#### The goal of the project:
 - Create a model that accurately predicts whether a trader should buy or sell a particular stock.
