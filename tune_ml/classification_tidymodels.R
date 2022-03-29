library(here)
library(janitor)
library(tidyverse)
library(tidymodels)

#LOAD DATA
load(here("tune_ml","compiled_data.RData"))

data_split <- initial_split(slide_filt, prop = 3/4)

train_data <- training(data_split)
test_data  <- testing(data_split)

posture <- recipe(code ~ ., data = train_data) %>% 
  update_role(id, new_role = "ID") %>% 
  step_zv(all_predictors())

rf_mod <- rand_forest(mode = "classification", trees = 150) %>%
  set_engine("randomForest") 

posture_wflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(posture)

posture_fit <- 
  posture_wflow %>% 
  fit(data = train_data)

posture_fit %>% 
  extract_fit_parsnip() 

posture_aug <- augment(posture_fit, test_data)

posture_aug %>% bal_accuracy(truth = code, .pred_class)

posture_aug %>% group_by(id) %>% 
  bal_accuracy(truth = code, .pred_class)

multi_metric <- metric_set(accuracy, kap, bal_accuracy, sens, spec)
posture_aug %>% multi_metric(truth = code, estimate = .pred_class)

