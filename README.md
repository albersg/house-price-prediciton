# House Prices Prediction

This repository contains an R script for predicting house prices using various regression models. The script performs exploratory data analysis, applies data transformations, fits regression models, and compares their performance using root mean squared error (RMSE). The analysis is performed on the "housing.csv" dataset, which contains information about various housing attributes.

## Dataset

The dataset used for this analysis is "housing.csv". It includes the following columns:

- CRIM: Per capita crime rate by town
- ZN: Proportion of residential land zoned for lots over 25,000 sq.ft.
- INDUS: Proportion of non-retail business acres per town
- CHAS: Charles River dummy variable (1 if tract bounds river; 0 otherwise)
- NOX: Nitric oxides concentration (parts per 10 million)
- RM: Average number of rooms per dwelling
- AGE: Proportion of owner-occupied units built prior to 1940
- DIS: Weighted distances to five Boston employment centers
- RAD: Index of accessibility to radial highways
- TAX: Full-value property tax rate per $10,000
- PTRATIO: Pupil-teacher ratio by town
- B: 1000(Bk - 0.63)^2 where Bk is the proportion of Black people by town
- LSTAT: Percentage lower status of the population
- PRICE: Median value of owner-occupied homes in $1000s

## Script Overview

1. Data Preparation and Exploration:
   - Install required packages.
   - Load necessary libraries.
   - Read the dataset.
   - Check the dimensions, data types, and missing values in the dataset.
   - Calculate summary statistics and visualize the data using correlation matrix, boxplots, and histograms.

2. Linear Regression Models:
   - Fit linear regression models using different combinations of predictor variables.
   - Evaluate model performance using summary statistics and visualizations.
   
3. Gradient Boosting:
   - Apply Box-Cox transformation to the target variable and select important variables.
   - Perform cross-validated grid search to find the optimal number of trees.
   - Fit a gradient boosting model using the optimal number of trees.
   - Evaluate model performance using RMSE.
   
4. Decision Tree, Random Forest, and SVM:
   - Fit decision tree, random forest, and support vector machine models.
   - Evaluate model performance using RMSE.
   
5. Model Comparison:
   - Compare the RMSE values of all the models.
   - Create a table summarizing the performance of each model.

## Results

The script provides a comprehensive analysis of house price prediction using different regression models. It compares the performance of linear regression, gradient boosting, decision tree, random forest, and support vector machine models. The RMSE values are calculated to assess the accuracy of the predictions.

Based on the analysis, the gradient boosting model with the optimal number of trees achieved the lowest RMSE, indicating the best performance among the models evaluated.

Please refer to the script and the generated outputs for detailed results and visualizations.

## License

This project is licensed under the [MIT License](LICENSE).

