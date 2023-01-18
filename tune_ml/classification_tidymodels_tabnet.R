library(here)
library(janitor)
library(torch)
library(tabnet)
library(tidyverse)
library(tidymodels)
library(finetune) # to use tuning functions from the new finetune package
library(vip) # to plot feature importances
library(doMC)

# tidymodels_prefer()
# i_am(".here")
# start_time <- Sys.time()
# 
# #Set cores for parallel processing
# # registerDoMC(cores = 12)
# 
# all_cores <- parallel::detectCores(logical = FALSE)

# library(doParallel)
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)

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
mod <- tabnet(epochs = 3, batch_size = 1024, decision_width = 32, attention_width = 16,
              num_steps = 5, penalty = 0.000001, feature_reusage = 1.5, learn_rate = 0.02) %>%
  set_engine("torch", verbose = TRUE) %>%
  set_mode("classification")

# Combine the pre-processed data and model into a workflow
posture_wflow <- 
  workflow() %>% 
  add_model(mod) %>% 
  add_recipe(posture)

# SINGLE MODEL

posture_fit <- posture_wflow %>% fit(train_data)

# Add the predictions to the test data
posture_aug <- augment(posture_fit, test_data)

#Overall performance
posture_aug %>% multi_metric(truth = code, estimate = .pred_class)

# Use the augmented model to calculate performance metrics
(posture_aug %>% group_by(id) %>% bal_accuracy(truth = code, .pred_class) -> balanced_acc_id)
posture_aug %>% group_by(id) %>% multi_metric(truth = code, estimate = .pred_class)
posture_aug %>% conf_mat(truth = code, estimate = .pred_class) %>% autoplot(type = "heatmap")

# TUNE PARAMETERS

mod <- tabnet(epochs = 3, batch_size = 1024, decision_width = tune(), attention_width = tune(),
              num_steps = tune(), penalty = 0.000001, feature_reusage = 1.5, learn_rate = tune()) %>%
  set_engine("torch", verbose = TRUE) %>%
  set_mode("classification")

posture_wflow <- 
  workflow() %>% 
  add_model(mod) %>% 
  add_recipe(posture)

grid <-
  posture_wflow %>%
  parameters() %>%
  update(
    decision_width = decision_width(range = c(20, 40)),
    attention_width = attention_width(range = c(20, 40)),
    num_steps = num_steps(range = c(4, 6)),
    learn_rate = learn_rate(range = c(-2.5, -1))
  ) %>%
  grid_max_entropy(size = 8)
grid

ctrl <- control_race(verbose_elim = TRUE)
folds <- vfold_cv(train_data, v = 5)
set.seed(777)

res <- posture_wflow %>%
  tune_race_anova(
    resamples = folds,
    grid = grid,
    control = ctrl
  )

res %>% show_best("accuracy") %>% select(- c(.estimator, .config))

# TUNE STEP 2

mod <- tabnet(epochs = 3, batch_size = tune(), decision_width = 22, attention_width = 27,
              num_steps = 5, penalty = tune(), feature_reusage = tune(), learn_rate = .0388) %>%
  set_engine("torch", verbose = TRUE) %>%
  set_mode("classification")

posture_wflow <- 
  workflow() %>% 
  add_model(mod) %>% 
  add_recipe(posture)

grid <-
  posture_wflow %>%
  parameters() %>%
  update(
    batch_size = batch_size(range = c(400^2,3000^2)),
    penalty = penalty(range = c(.00001,0.01)),
    feature_reusage = feature_reusage(range = c(1,2))
  ) %>%
  grid_max_entropy(size = 8)
grid

ctrl <- control_race(verbose_elim = TRUE)
folds <- vfold_cv(train_data, v = 5)
set.seed(777)

res <- posture_wflow %>%
  tune_race_anova(
    resamples = folds,
    grid = grid,
    control = ctrl
  )

res %>% show_best("accuracy") %>% select(- c(.estimator, .config))

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



