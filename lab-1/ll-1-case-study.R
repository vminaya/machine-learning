## ----setup, include=FALSE
## CLASSIFICATION MODELS 
## 
## -----------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)


## -----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(janitor)


## -----------------------------------------------------------------------------------------------------------
d <- read_csv("data/ngsschat-processed-data.csv")

d


## -----------------------------------------------------------------------------------------------------------



## -----------------------------------------------------------------------------------------------------------
set.seed(20220712)
train_test_split <- initial_split(d, prop = .70) #0.80 proportion for training and 20% for testing. If small data, this number goes down; if large is closer to 90
data_train <- training(train_test_split)


## -----------------------------------------------------------------------------------------------------------



## -----------------------------------------------------------------------------------------------------------
my_rec <- recipe(code ~ ., data = data_train) #engineering features: dependent variable is code


## -----------------------------------------------------------------------------------------------------------
# specify model
my_mod <-
    logistic_reg() %>% 
    set_engine("glm") %>% #gml kinf of model
    set_mode("classification")  #type of work for categorizing a dependent variable (predicting ts of sv; classification); "regression" if predicting a number


## Specify recipe, and workflow-----------------------------------------------------------------------------------------------------------
my_wf <-
    workflow() %>%
    add_model(my_mod) %>% 
    add_recipe(my_rec)


## ---- warning = FALSE---------------------------------------------------------------------------------------
fitted_model <- fit(my_wf, data = data_train)


## ---- include = FALSE---------------------------------------------------------------------------------------
final_fit <- last_fit(fitted_model, train_test_split)


## -----------------------------------------------------------------------------------------------------------


## -----------------------------------------------------------------------------------------------------------
# collect test split predictions
final_fit %>%
    collect_predictions()


## -----------------------------------------------------------------------------------------------------------
final_fit %>% 
    collect_predictions() %>% # see test set predictions
    select(.pred_class, code) %>% # just to make the output easier to view 
    mutate(correct = .pred_class == code) # create a new variable, correct, telling us when the model was and was not correct


## -----------------------------------------------------------------------------------------------------------
final_fit %>% 
    collect_predictions() %>% # see test set predictions
    select(.pred_class, code) %>% # just to make the output easier to view 
    mutate(correct = .pred_class == code) %>% # create a new variable, correct, telling us when the model was and was not correct
    tabyl(correct)


## -----------------------------------------------------------------------------------------------------------
final_fit %>% 
    collect_metrics()

