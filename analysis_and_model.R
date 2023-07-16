install.packages("tidyverse")
install.packages("corrplot")
install.packages("gridExtra")
install.packages("lmtest")
install.packages("tseries")
install.packages("splitTools")
install.packages("Metrics")
install.packages("gbm")

library(tidyverse)
library(corrplot)
library(gridExtra)
library(lmtest)
library(tseries)
library(dplyr)
library(splitTools)
library(Metrics)
library(gbm)

headers <- c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 'DIS', 'RAD', 'TAX', 'PTRATION', 'B', 'LSTAT', 'PRICE')

# We don't specify sep = " ", because the csv can be separated by a variable amount of spaces
# (one or more), so with sep = "", R knows that can find one or more whites.
df <- read.table('../house_prices_prediciton/housing.csv', col.names = headers)


# CRIM: Per capita crime rate by town.
# ZN: Proportion of residential land zoned for lots over 25,000 sq.ft.
# INDUS: Proportion of non-retail business acres per town.
# CHAS: Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# NOX: Nitric oxides concentration (parts per 10 million).
# RM: Average number of rooms per dwelling.
# AGE: Proportion of owner-occupied units built prior to 1940.
# DIS: Weighted distances to five Boston employment centres.
# RAD: Index of accessibility to radial highways.
# TAX: Full-value property-tax rate per $10,000.
# PTRATIO: Pupil-teacher ratio by town.
# B: 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# LSTAT: Percentage of lower status of the population.
# MEDV: Median value of owner-occupied homes in $1000's.

# Check the dimensions of the dataset
dim(df)

# Check the types of the data
str(df)

# NaN values are always a problem. Let's see if there are any of them.
sum(is.na(df))

# No NaN values, great!

summary(df)

m<-cor(df)
corrplot(m, method="number")


# Let's the highest correlations in the matrix, in a list format
# https://stackoverflow.com/questions/7074246/show-correlations-as-an-ordered-list-not-as-a-large-matrix

m[lower.tri(m,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
m=as.data.frame(as.table(m))  #Turn into a 3-column table
m=na.omit(m)  #Get rid of the junk we flagged above
m=m[order(-abs(m$Freq)),]    #Sort by highest correlation (whether +ve or -ve)


# The most significant values are the next ones
subset(m, abs(Freq) > 0.5)

# Mainly, RAD-TAX, NOX-DIS and INDUX-NOX are some of the higher
# values that we have in the correlation matrix, higher that 0.75

plots <- df %>% 
  pivot_longer(cols = -PRICE) %>% 
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Variable", y = "Value")

print(plots)

plots <- df %>% 
  pivot_longer(cols = -PRICE) %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Value", y = "Frequency")

print(plots)

# Like we can see in both graphics, the RM column has almost a normal distribution.

# We saw in the correlation matrix that the two variables with higher values with Price
# were LSTAT and RM

model_rm <- lm(PRICE ~ RM, data = df)
model_lstat <- lm(PRICE ~ LSTAT, data = df)

summary(model_lstat)

# LSTAT has a higher R^2, and a lower "Residual standard error", so that is the selected model

df %>% 
  ggplot(aes(x = LSTAT, y = PRICE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "RM", y = "PRICE")

plot(model_lstat)

# We have seen simple models with one unique variable trying to predict the price.
# Now, we are going to try to get better results.


# Let's the most related with Price

m_filtered <- m %>% 
  filter(Var1 == "PRICE" | Var2 == "PRICE")

# Get only the most important variables

df <- df %>% select(RM, LSTAT, PTRATION, PRICE)

# Split in train, validation and set

set.seed(123)

train_percentage <- 0.7
validation_percentage <- 0.2
test_percentage <- 0.1

num_rows <- nrow(df)
num_train <- floor(train_percentage * num_rows)
num_validation <- floor(validation_percentage * num_rows)
num_test <- num_rows - num_train - num_validation

train_indices <- sample(seq_len(num_rows), size = num_train)
remaining_indices <- setdiff(seq_len(num_rows), train_indices)
validation_indices <- sample(remaining_indices, size = num_validation)
test_indices <- setdiff(remaining_indices, validation_indices)

train_set <- df[train_indices, ]
validation_set <- df[validation_indices, ]
test_set <- df[test_indices, ]

linear_model <- lm(PRICE ~ .,data=train_set) 
prediction = predict(linear_model, test_set[, 1:3])

lm_rmse <- rmse(test_set$PRICE, prediction)

gradient_boost <- gbm(PRICE ~ . ,data = train_set, distribution = "gaussian", 
                      n.trees = 10000, shrinkage = 0.01,  interaction.depth = 4)

n.trees = seq(from=100 ,to=10000, by=100) 

pred_matrix <- predict(gradient_boost, test_set[, 1:3],n.trees = n.trees)

rmses <- apply(pred_matrix,2,function(p){
  rmse(test_set$PRICE,p)
})

plot(rmses)

lowest_rmse <- min(rmses)


