# Sales_Prediction_R


Sales Prediction
In many businesses, identifying which customers will make a purchase (and when), is a critical exercise.
This is true for both brick-and-mortar outlets and online stores. The data provided in this assignment
is website traffic data acquired from an online retailer. You will be predicting customer sales.
The data provides information on customer’s website site visit behavior. Customers may visit the store
multiple times, on multiple days, with or without making a purchase.
Your ultimate goal is to predict how much sales revenue can be expected from each customer. The
variable revenue lists the amount of money that a customer spends on a given visit. Your goal is to
predict how much money a customer will spend, in total, across all visits to the website, during the
allotted one-year time frame (August 2016 to August 2017).
More specifically, you will need to predict a transformation of the aggregrate customer-level sales value
based on the natural log. That is, if customer i has ki revenue transactions, then you should compute:

And then transform this variable as follows:
targetRevenuei = ln(custRevenuei + 1) ∀i ∈customers
- Conduct a thorough exploratory data analysis (EDA) 
- Data preparation. Choose five categories of data preparation tasks for the data. For
each, state the method and describe/visualize the results. The categories you may choose from for
include:
• missing value imputation
• resolution of outliers
• aggregations
• transformations
• collapsing categories
• creating new categories or binning
• feature engineering or feature extraction
You may wish to consider using the dplyr, lubridate, and/or forcats packages, among others,
to help you manage this process.
- Modelling - OLS model, robust regression, lasso, ridge
- Model Performance 
