
# House Price Prediction using R

This project aims to analyze and predict house prices using a dataset containing various features of houses. The analysis is performed using R, leveraging data exploration, visualization, cleaning, and machine learning algorithms. This project can be beneficial for individuals looking to make informed decisions in the real estate market.

## Dataset

The dataset used is `house_data.csv` which contains 1460 observations and 51 variables. These variables include both numerical and categorical data relevant to housing features, such as living area, number of bedrooms, and sale price.

## Steps Involved

### 1. Data Exploration
- Loaded the dataset and viewed its dimensions and structure.
- Summarized numerical and categorical variables.
- Visualized key variables, such as Sale Price and Living Area, to identify trends and patterns.

### 2. Data Cleaning
- Identified missing values and outliers in the dataset.
- Handled missing values using imputation techniques such as mean, median, and mode, depending on the context of the data.
- Removed outliers using Inter Quartile Range (IQR) and boxplots.

### 3. Correlation Analysis
- Performed correlation analysis to identify relationships between variables.
- Created a correlation plot to visualize highly correlated variables.

### 4. Machine Learning Models
Implemented various machine learning models to predict house prices and categorize the condition of houses:

#### a. Logistic Regression (Multinomial)
- Predicted the overall condition of houses using a newly created categorical variable, `New_cond`.
- Achieved an accuracy of 95.9% for predicting house conditions.

#### b. Naive Bayes Classifier
- Applied to predict the overall condition of houses.
- Achieved an accuracy of 83.5%.

#### c. Linear Regression
- Predicted the sale price of houses.
- Visualized the difference between actual and predicted prices using ggplot2.

#### d. Random Forest
- Applied Random Forest to predict the sale price, achieving better accuracy with a lower RMSE compared to linear regression.

### 5. Resampling Methods
- Used cross-validation and bootstrapping to validate the model's performance and ensure generalization to new data.

## Key Findings
- The `OverallQual` and `GrLivArea` features were highly correlated with the sale price.
- Random Forest outperformed Linear Regression in predicting house prices.
- Logistic Regression provided accurate predictions for house conditions.

## Conclusion
This project successfully predicts house prices and categorizes house conditions using various machine learning techniques in R. The results can assist individuals in making informed decisions in the real estate market, such as buying or selling properties.

## Future Work
- Implement additional models such as Gradient Boosting.
- Explore feature engineering to improve prediction accuracy.
- Extend the analysis to include more complex datasets.
