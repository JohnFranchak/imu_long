library(here)
library(janitor)
library(tidyverse)
library(tidymodels)
# library(doMC)
tidymodels_prefer()
i_am(".here")
start_time <- Sys.time()

#Set cores for parallel processing
# registerDoMC(cores = 12)

all_cores <- parallel::detectCores(logical = FALSE)

library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

# Custom set of multi-class metrics
multi_metric <- metric_set(accuracy, kap, bal_accuracy, f_meas, ppv, sens, spec)

#LOAD DATA
load( here("code","tune_ml","compiled_data_lite.RData"))
load( here("tune_ml","compiled_data_lite.RData"))

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
(posture_aug %>% group_by(id) %>% bal_accuracy(truth = code, .pred_class) -> balanced_acc_id)
posture_aug %>% multi_metric(truth = code, estimate = .pred_class)
posture_aug %>% conf_mat(truth = code, estimate = .pred_class) %>% autoplot(type = "heatmap")

posture_aug %>% group_by(id) %>% multi_metric(truth = code, estimate = .pred_class) %>% write_csv("tune_ml/splithalf-metrics.csv")


# TUNING

rf_mod <- rand_forest(mode = "classification",
                      mtry = tune(),
                      trees = tune(),
                      min_n = tune()) %>%
  set_engine("randomForest") 


posture_wflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(posture)

grid <-
  posture_wflow %>%
  parameters() %>%
  update(
    mtry = mtry(range = c(35, 100)),
    trees = trees(range = c(400, 800)),
    min_n = min_n(range = c(2, 6))
  ) %>%
  grid_max_entropy(size = 8)
grid
# mtry trees min_n .metric   mean     n  std_err
# 44   550     4   accuracy 0.971     5 0.000557

ctrl <- control_race(verbose = TRUE, verbose_elim = TRUE)
folds <- vfold_cv(train_data, v = 5)
set.seed(777)

res <- posture_wflow %>%
  tune_race_anova(
    resamples = folds,
    grid = grid,
    control = ctrl
  )

res %>% show_best("accuracy") %>% select(- c(.estimator, .config))

# Based on tuning
rf_mod <- rand_forest(mode = "classification",
                      mtry = 29,
                      trees = 1625,
                      min_n = 6) %>%
  set_engine("randomForest") 

posture_wflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(posture)

# RESAMPLING

# Resample by ID
split_by_id <- group_vfold_cv(train_data, group = "id")

# Resampling workflow
posture_fit_rs <- 
  posture_wflow %>% 
  fit_resamples(split_by_id, metrics = multi_metric, 
                control = control_resamples(save_pred = T, parallel_over = "everything", verbose = T))

metrics <- collect_metrics(posture_fit_rs, summarize = F)
metrics %>% group_by(.metric) %>% rstatix::get_summary_stats(.estimate)
# collect_predictions(posture_fit_rs)

pred_rs <- collect_predictions(posture_fit_rs, summarize = TRUE)
pred_rs$truth <- train_data$code

conf_mat_resampled(posture_fit_rs, tidy = F) %>% autoplot(type = "heatmap")

end_time <- Sys.time()
elapsed <- end_time - start_time

ggplot(metrics, aes(.estimate)) + geom_histogram() + facet_wrap(".metric", scales = "free") + xlim(0,1)

write_csv(pred_rs, "tune_ml/predictions-ground-truth.csv")
write_csv(metrics, "tune_ml/resampled-metrics.csv")
save(elapsed, posture_fit_rs, metrics, file =  here("code","tune_ml","classification_tidymodels_output.RData"))

