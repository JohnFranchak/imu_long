library(here)
library(janitor)
library(tidyverse)
library(tidymodels)
library(doMC)
tidymodels_prefer()
# i_am(".here")
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

slide_filt <- slide_filt %>% 
  mutate(code = factor(ifelse(code == "Sitting", "Sitting", "Not Sitting")))

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
posture_aug %>% multi_metric(truth = factor(code), estimate = .pred_class)
posture_aug %>% group_by(id) %>% multi_metric(truth = factor(code), estimate = .pred_class) %>% arrange(.estimate)

#Classic RF with all data
require(randomForest)

not_all_na <- function(x) any(!is.na(x))
training <- slide_filt %>% select_if(not_all_na)
training$code <- factor(training$code)
rfmodel <- randomForest(code ~ ., data = training, localImp = TRUE, proximity = FALSE, ntree = 150)
save(rfmodel, file =  here("binary_models","group_model_sitting.RData"))


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

save(elapsed, posture_fit_rs, metrics, file =  here("code","tune_ml","classification_tidymodels_output.RData"))

