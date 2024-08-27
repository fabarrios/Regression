# Example of tidy models, from r-bloggers
# Works with the wine data quality for machine learning.
# Tidy models are a set of wrapper functions to execute ML projects and models.
library(vip)
library(readr)
library(dplyr)
library(tidymodels)
library(rpart.plot)

# Directly load the csv file from the WEB
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
wine_data <- read_delim(url, col_names = TRUE, delim = ";")

head(wine_data)

# Look at the wien quality clasification
wine_data %>%
  group_by(quality) %>%
  count()

# it will help to group into less qualities
wine_data <- wine_data %>%
  mutate(quality_fct = case_when(
    quality %in% c(3, 4) ~ "bad",
    quality %in% c(5, 6) ~ "good",
    quality %in% c(7, 8) ~ "great"
  )) %>%
  select(-quality)
wine_data$quality_fct <- factor(wine_data$quality_fct, levels = c("bad", "good", "great"))
wine_data %>%
  group_by(quality_fct) %>%
  count()

# Machine learning pipeline
# The initial_split will let you select the portion of the data that’ll belong 
# to the training set and for the test set. It allows you to control stratification.
set.seed(42)
split <- initial_split(wine_data, prop = 0.8, strata = quality_fct)
train_data <- training(split)
test_data <- testing(split)

dim(train_data)
dim(test_data)

# The models
# Start by normalize numeric data to have a mean of 0 and a standard deviation of 1.
# Before you have to specify how the data will be modeled with a model equation. Left 
# part contains the target variable, and the right part contains the features (the
# dot indicates you want to use all features)
wine_recipe <-
  recipe(quality_fct ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors())
wine_recipe

# 11 predictor variables which should be scaled before proceeding.
# First declare the model, a decision tree sounds like a good option since you’re 
# dealing with a multi-class classification dataset.
wine_model <-
  decision_tree(mode = "classification") %>%
  set_engine("rpart")

# now the work flow to train a machine learning model.
wine_workflow <-
  workflow() %>%
  add_model(wine_model) %>%
  add_recipe(wine_recipe)
wine_workflow

# And then the model fitting. Chaining a `fit()` function to the workflow. with
# the fitting with the training dataset.
wine_fit <-
  wine_workflow %>%
  fit(data = train_data)
wine_fit

# How good your model is? First see how the model makes decisions and which features
# it considers to be most important.
# The code to represent as a chart the text from the decision tree text fit (the 
# model fit)
wine_fit %>%
  extract_fit_engine() %>%
  rpart.plot()

# And the importance of the features
wine_fit %>%
  extract_fit_parsnip() %>%
  vip()

# Prediction Evaluation
# The best way to understand it is by calculating predictions on the test set 
# (previously unseen data) and evaluating it against true values, the impact 
# on prediction quality. Tidymodels uses the `yardstick` package to 
# implement evaluation metrics.
wine_preds <-
  augment(wine_fit, test_data) %>%
  select(quality_fct, .pred_class, .pred_bad, .pred_good, .pred_great) %>%
  rename(
    actual = quality_fct,
    predicted = .pred_class
  )
wine_preds

# To extract the accuracy, precision, and recall:
wine_preds %>% accuracy(truth = actual, predicted)
wine_preds %>% precision(truth = actual, predicted)
wine_preds %>% recall(truth = actual, predicted)

# Confusion matrix
wine_preds %>%
  conf_mat(truth = actual, predicted)

wine_preds %>%
  summary()

# And the plots per class
# For understanding the model fitting quality, ROC (Reciever Operating 
# Characteristics) curve it plots the true positive rate on the x-axis 
# and the false positive rate on the y-axis
# 1 Each point on the graph corresponds to a different classification threshold.
# 2 The dotted diagonal line represents a random classifier.
# 3 The curve should rise from this diagonal line, indicating that 
#   predictive modeling makes sense.
# 4 The area under this curve measures the overall performance of the 
#   classifier.
# The PR (Precision-Recall) curve. It plots precision (y-axis) against recall 
# (x-axis) for different classification thresholds to show you the trade-offs 
# between these two metrics.
# A curve that’s close to the top-right corner indicates the model performs 
# well (high precision and recall).
# A curve that’s L-shaped suggests that a model has a good balance of precision 
# and recall for a subset of thresholds, but performs poorly outside this range.
# A flat curve represents a model that’s not sensitive to different threshold 
# values, as precision and recall values are consistent.
wine_preds %>%
  roc_curve(truth = actual, .pred_bad:.pred_great) %>%
  autoplot()

wine_preds %>%
  pr_curve(truth = actual, .pred_bad:.pred_great) %>%
  autoplot()