# Regression Model Selection Using R
## Introduction
This script demonstrates the use of regression model selection techniques in R. The script uses the College dataset from the ISLR2 package. The goal is to predict the number of college applications received based on various features of the college such as acceptance rate, SAT scores, etc.

## Libraries
The script uses the following libraries:

1. ISLR2
2. glmnet
3. pls
4. leaps

## Data Preparation
The first step is to clean the data and remove any missing values. The data is then scaled to have zero mean and unit variance. The data is split into two parts, a training set and a test set. The training set is used to fit the models and the test set is used to evaluate the models.

## Model Selection
The script uses several model selection techniques to find the best model for the data. The techniques used are:

1. Best Subset Selection
2. Forward Stepwise Selection
3. Backward Stepwise Selection
4. Cross-Validation
The best model is selected based on three criteria:

1. Adjusted R-Squared
2. Cp (Mallows Cp)
3. BIC (Bayesian Information Criterion)
## Results
The best model is found to have 10 predictors and has the lowest mean cross-validation error. The predictors in the best model are:

1. Private
2. Outstate
3. Elite
4. S.F.Ratio
5. Expend
6. Grad.Rate
7. Room.Board
8. Books
9. Personal
10. PhD
## Conclusion
This script demonstrates the use of regression model selection techniques in R. The script uses the College dataset from the ISLR2 package to predict the number of college applications received based on various features of the college. The best model is found to have 10 predictors and has the lowest mean cross-validation error.
