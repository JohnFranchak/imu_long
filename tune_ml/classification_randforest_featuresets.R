require(here)
require(randomForest)
library(randomForestExplainer)
require(cvms)
require(caret)
require(tidyverse)
require(glue)
i_am(".here")

#LOAD DATA
load("tune_ml/compiled_data_lite.RData")

training <- slide_filt %>% group_by(code) %>% slice_head(prop = .75) %>% ungroup %>% select(-id)
testing <- slide_filt %>% group_by(code) %>% slice_tail(prop = .25) %>% ungroup %>% select(-id)

not_all_na <- function(x) any(!is.na(x))
training <- training %>% select_if(not_all_na)

# prediction function
get_metrics <- function(rfmodel, testing) {
  predictions <- predict(rfmodel, testing, type = "class")
  u <- union(predictions, testing$code)
  res <- confusion_matrix(factor(testing$code, u),factor(predictions, u))
  print(res$`Balanced Accuracy`)
  print(res$F1)
  print(res$`Table`)
  res
}

# Baseline = 95% balanced accuracy, 92% F1 score
rfmodel <- randomForest(code ~ ., data = training, localImp = TRUE, proximity = FALSE, ntree = 150)
get_metrics(rfmodel, testing)

# Explore parameters
min_depth_frame <- min_depth_distribution(rfmodel)
plot_min_depth_distribution(min_depth_frame, k = 50)

#Single sensor/axis features only, 94 balanced and 90.5 F1 score
training_single <- training %>% select(code:ragyr_MAG)
rfmodel_single <- randomForest(code ~ ., data = training_single, localImp = TRUE, proximity = FALSE, ntree = 150)
get_metrics(rfmodel_single, testing)

# Only acceleration, 91% balanced an 85% F score
training_acc <- training %>% select(code, contains("acc"))
rfmodel_acc <- randomForest(code ~ ., data = training_acc, localImp = TRUE, proximity = FALSE, ntree = 150)
get_metrics(rfmodel_acc, testing)


# Only gyro is quite bad, 80% balanced and 69% F score
training_gyr <- training %>% select(code, contains("gyr"))
rfmodel_gyr <- randomForest(code ~ ., data = training_gyr, localImp = TRUE, proximity = FALSE, ntree = 150)
get_metrics(rfmodel_gyr, testing)


