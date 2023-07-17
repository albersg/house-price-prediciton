# Install required packages
#install.packages(c("tidyverse", "corrplot", "gridExtra", "lmtest", "tseries", "splitTools", "Metrics", "gbm", "rpart", "randomForest", "e1071", "MASS", "forecast")

# Load libraries
library(tidyverse)
library(corrplot)
library(gridExtra)
library(lmtest)
library(tseries)
library(dplyr)
library(splitTools)
library(Metrics)
library(gbm)
library(rpart)
library(randomForest)
library(e1071)
library(MASS)
library(forecast)

# Define column headers
headers <- c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT', 'PRICE')

# Read the dataset
df <- read.table('../house_prices_prediciton/housing.csv', col.names = headers, sep = "")

# Check the dimensions of the dataset
dim(df)

# Check the types of the data
str(df)

# Check for NaN values
sum(is.na(df))

# Summary statistics of the dataset
summary(df)

# Calculate correlation matrix
m <- cor(df)
corrplot(m, method = "number")

# Extract the highest correlations from the matrix
m[lower.tri(m, diag = TRUE)] <- NA
m <- as.data.frame(as.table(m))
m <- na.omit(m)
m <- m[order(-abs(m$Freq)),]

# Print correlations with absolute values greater than 0.5
subset(m, abs(Freq) > 0.5)

# Create boxplot for each variable
plots <- df %>% 
  pivot_longer(cols = -PRICE) %>% 
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Variable", y = "Value")
print(plots)

# Create histogram for each variable
plots <- df %>% 
  pivot_longer(cols = -PRICE) %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Value", y = "Frequency")
print(plots)

# Fit linear regression models and compare them
model_rm <- lm(PRICE ~ RM, data = df)
model_lstat <- lm(PRICE ~ LSTAT, data = df)
summary(model_lstat)

# Visualize the relationship between LSTAT and PRICE
df %>% 
  ggplot(aes(x = LSTAT, y = PRICE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "LSTAT", y = "PRICE")
plot(model_lstat)

# Select important variables and split the dataset into train, validation, and test sets
m_filtered <- m %>%
  filter(Var1 == "PRICE" | Var2 == "PRICE")

# Apply Box-Cox transformation to the selected variables
lambda_PRICE <- BoxCox.lambda(df$PRICE, method = "loglik", lower = -5, upper = 5)
lambda_RM <- BoxCox.lambda(df$RM, method = "loglik", lower = -5, upper = 5)
lambda_LSTAT <- BoxCox.lambda(df$LSTAT, method = "loglik", lower = -5, upper = 5)
lambda_PTRATIO <- BoxCox.lambda(df$PTRATIO, method = "loglik", lower = -5, upper = 5)

# Transformed variables
PRICE_BC <- BoxCox(df$PRICE, lambda_PRICE)
RM_BC <- BoxCox(df$RM, lambda_RM)
LSTAT_BC <- BoxCox(df$LSTAT, lambda_LSTAT)
PTRATIO_BC <- BoxCox(df$PTRATIO, lambda_PTRATIO)

# Add transformed variables to the dataframe
df$PRICE_BC <- PRICE_BC
df$RM_BC <- RM_BC
df$LSTAT_BC <- LSTAT_BC
df$PTRATIO_BC <- PTRATIO_BC

# Select variables for the models
df_selected <- df %>% 
  dplyr::select(PRICE_BC, RM_BC, LSTAT_BC, PTRATIO_BC, PRICE, RM, LSTAT, PTRATIO)

# Split into train, validation, and test sets
set.seed(123)

train_percentage <- 0.7
validation_percentage <- 0.2
test_percentage <- 0.1

num_rows <- nrow(df_selected)
num_train <- floor(train_percentage * num_rows)
num_validation <- floor(validation_percentage * num_rows)
num_test <- num_rows - num_train - num_validation

train_indices <- sample(seq_len(num_rows), size = num_train)
remaining_indices <- setdiff(seq_len(num_rows), train_indices)
validation_indices <- sample(remaining_indices, size = num_validation)
test_indices <- setdiff(remaining_indices, validation_indices)

train_set <- df_selected[train_indices, ]
validation_set <- df_selected[validation_indices, ]
test_set <- df_selected[test_indices, ]

# Find optimal n.trees for Gradient Boosting
gbm_fit <- gbm(PRICE ~ RM + LSTAT + PTRATIO, 
               data = train_set, 
               distribution = "gaussian",
               n.trees = 10000,
               interaction.depth = 4,
               shrinkage = 0.01,
               cv.folds = 5)

# Get optimal n.trees value
optimal_ntrees <- gbm.perf(gbm_fit, method = "cv")

# Models with original variables
model_lm_orig <- lm(PRICE ~ RM + LSTAT + PTRATIO, data = train_set)
prediction_lm_orig <- predict(model_lm_orig, test_set)
rmse_lm_orig <- rmse(test_set$PRICE, prediction_lm_orig)

model_gb_orig <- gbm(PRICE ~ RM + LSTAT + PTRATIO, data = train_set, distribution = "gaussian", n.trees = optimal_ntrees, shrinkage = 0.01, interaction.depth = 4)
prediction_gb_orig <- predict(model_gb_orig, test_set, n.trees = optimal_ntrees)
rmse_gb_orig <- rmse(test_set$PRICE, prediction_gb_orig)

model_tree_orig <- rpart(PRICE ~ RM + LSTAT + PTRATIO, data = train_set)
prediction_tree_orig <- predict(model_tree_orig, test_set)
rmse_tree_orig <- rmse(test_set$PRICE, prediction_tree_orig)

model_rf_orig <- randomForest(PRICE ~ RM + LSTAT + PTRATIO, data = train_set)
prediction_rf_orig <- predict(model_rf_orig, test_set)
rmse_rf_orig <- rmse(test_set$PRICE, prediction_rf_orig)

model_svm_orig <- svm(PRICE ~ RM + LSTAT + PTRATIO, data = train_set)
prediction_svm_orig <- predict(model_svm_orig, test_set)
rmse_svm_orig <- rmse(test_set$PRICE, prediction_svm_orig)

# Models with transformed variables
model_lm_transformed <- lm(PRICE_BC ~ RM_BC + LSTAT_BC + PTRATIO_BC, data = train_set)
prediction_lm_transformed <- predict(model_lm_transformed, test_set)
rmse_lm_transformed <- rmse(test_set$PRICE_BC, prediction_lm_transformed)

model_gb_transformed <- gbm(PRICE_BC ~ RM_BC + LSTAT_BC + PTRATIO_BC, data = train_set, distribution = "gaussian", n.trees = optimal_ntrees, shrinkage = 0.01, interaction.depth = 4)
prediction_gb_transformed <- predict(model_gb_transformed, test_set, n.trees = optimal_ntrees)
rmse_gb_transformed <- rmse(test_set$PRICE_BC, prediction_gb_transformed)

model_tree_transformed <- rpart(PRICE_BC ~ RM_BC + LSTAT_BC + PTRATIO_BC, data = train_set)
prediction_tree_transformed <- predict(model_tree_transformed, test_set)
rmse_tree_transformed <- rmse(test_set$PRICE_BC, prediction_tree_transformed)

model_rf_transformed <- randomForest(PRICE_BC ~ RM_BC + LSTAT_BC + PTRATIO_BC, data = train_set)
prediction_rf_transformed <- predict(model_rf_transformed, test_set)
rmse_rf_transformed <- rmse(test_set$PRICE_BC, prediction_rf_transformed)

model_svm_transformed <- svm(PRICE_BC ~ RM_BC + LSTAT_BC + PTRATIO_BC, data = train_set)
prediction_svm_transformed <- predict(model_svm_transformed, test_set)
rmse_svm_transformed <- rmse(test_set$PRICE_BC, prediction_svm_transformed)

# Create error (RMSE) comparison table
model_names <- c("Linear Regression", "Gradient Boosting", "Decision Tree", "Random Forest", "SVM")
rmse_values_orig <- c(rmse_lm_orig, rmse_gb_orig, rmse_tree_orig, rmse_rf_orig, rmse_svm_orig)
rmse_values_transformed <- c(rmse_lm_transformed, rmse_gb_transformed, rmse_tree_transformed, rmse_rf_transformed, rmse_svm_transformed)

comparison <- data.frame(Model = model_names, RMSE_Original = rmse_values_orig, RMSE_Transformed = rmse_values_transformed)
comparison

#         Model         RMSE_Original  RMSE_Transformed
# 1 Linear Regression      6.292968        0.4432498
# 2 Gradient Boosting      4.453148        0.3965086
# 3     Decision Tree      4.621702        0.4086809
# 4     Random Forest      4.554756        0.3951336
# 5               SVM      4.838435        0.3967701
