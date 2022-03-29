library(here)
library(janitor)
library(tidyverse)
library(tidymodels)
library(doMC)

registerDoMC(cores = 8)

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

#EVAL SINGLE MODEL
posture_fit %>% 
  extract_fit_parsnip() 

posture_aug <- augment(posture_fit, test_data)

posture_aug %>% bal_accuracy(truth = code, .pred_class)

posture_aug %>% group_by(id) %>% 
  bal_accuracy(truth = code, .pred_class)

multi_metric <- metric_set(accuracy, kap, bal_accuracy, sens, spec)
posture_aug %>% multi_metric(truth = code, estimate = .pred_class)

posture_aug %>% group_by(id) %>% multi_metric(truth = code, estimate = .pred_class)
posture_aug %>% conf_mat(truth = code, estimate = .pred_class) %>% autoplot(type = "heatmap")

#RESAMPLING
split_by_id <- group_vfold_cv(train_data, group = "id")
posture_fit_rs <- 
  posture_wflow %>% 
  fit_resamples(split_by_id, metrics = multi_metric, control = control_resamples(save_pred = T, parallel_over = "everything"))
collect_metrics(posture_fit_rs, summarize = F) %>% filter(.metric == "bal_accuracy")
