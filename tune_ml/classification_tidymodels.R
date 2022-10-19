library(here)
library(janitor)
library(tidyverse)
library(tidymodels)
library(doMC)
tidymodels_prefer()

start_time <- Sys.time()

#Set cores for parallel processing
registerDoMC(cores = 12)

# Custom set of multi-class metrics
multi_metric <- metric_set(accuracy, kap, bal_accuracy, sens, spec)

#LOAD DATA
load(here("tune_ml","compiled_data_lite.RData"))

#Split data into train/test, ensuring all ids appear in both
data_split <- initial_split(slide_filt, prop = 3/4, strata = "id")
train_data <- training(data_split)
test_data  <- testing(data_split)

# Create a recipe for pre-processing the training data
posture <- recipe(code ~ ., data = train_data) %>% 
  update_role(id, new_role = "ID") %>% 
  step_zv(all_predictors())

# Create a random forest model to classify
rf_mod <- rand_forest(mode = "classification") %>%
  set_engine("randomForest") 

# Combine the pre-processed data and model into a workflow
posture_wflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(posture)

# SINGLE MODEL

posture_fit <- 
  posture_wflow %>% 
  fit(data = train_data)

posture_fit %>% extract_fit_parsnip() 

# Add the predictions to the test data
posture_aug <- augment(posture_fit, test_data)

# Use the augmented model to calculate performance metrics
posture_aug %>% group_by(id) %>% bal_accuracy(truth = code, .pred_class)
posture_aug %>% multi_metric(truth = code, estimate = .pred_class)
posture_aug %>% group_by(id) %>% multi_metric(truth = code, estimate = .pred_class)
posture_aug %>% conf_mat(truth = code, estimate = .pred_class) %>% autoplot(type = "heatmap")

# RESAMPLING

# Resample by ID
split_by_id <- group_vfold_cv(train_data, group = "id")

# Resampling workflow
posture_fit_rs <- 
  posture_wflow %>% 
  fit_resamples(split_by_id, metrics = multi_metric, 
                control = control_resamples(save_pred = T, parallel_over = "everything", verbose = T))

metrics <- collect_metrics(posture_fit_rs, summarize = F) %>% filter(.metric == "bal_accuracy")
collect_predictions(posture_fit_rs)

end_time <- Sys.time()
elapsed <- end_time - start_time
save(elapsed, posture_fit_rs, metrics, file = "classification_tidymodels_output.RData")

