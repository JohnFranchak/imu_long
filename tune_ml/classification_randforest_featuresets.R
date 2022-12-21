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

training <- slide_filt %>% group_by(id, code) %>% slice_head(prop = .75) %>% ungroup %>% select(-id)
testing <- slide_filt %>% group_by(id, code) %>% slice_tail(prop = .25) %>% ungroup 

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

# Baseline = 95% balanced accuracy, 91% F1 score
rfmodel <- randomForest(code ~ ., data = training, localImp = TRUE, proximity = FALSE, ntree = 100)
get_metrics(rfmodel, testing)

# Explore parameters
min_depth_frame <- min_depth_distribution(rfmodel)
plot_min_depth_distribution(min_depth_frame, k = 50)

#By individuals
mets <- function(rfmodel, test_set, ppt) {
  predictions <- predict(rfmodel, filter(test_set, id == ppt), type = "class")
  test_id <- test_set %>% filter(id == ppt)
  u <- union(predictions, test_id$code)
  res <- confusion_matrix(factor(test_id$code, u),factor(predictions, u))
  select(res, -(`Confusion Matrix`:`Class Level Results`)) %>% mutate(id = ppt, .before = `Overall Accuracy`)
}
mets_id <- unique(testing$id) %>% map_dfr(~ mets(rfmodel = rfmodel, test_set = testing, .x))

#Check data quality
slide_filt %>% ggplot(aes(x = factor(id), y = CORR_lhacc_x_rhacc_x)) + geom_boxplot()
slide_filt %>% ggplot(aes(x = factor(id), y = CORR_lhacc_y_rhacc_y)) + geom_boxplot()
slide_filt %>% ggplot(aes(x = factor(id), y = CORR_lhacc_z_rhacc_z)) + geom_boxplot()

slide_filt %>% ggplot(aes(x = factor(id), y = CORR_laacc_x_raacc_x)) + geom_boxplot()
slide_filt %>% ggplot(aes(x = factor(id), y = CORR_laacc_z_raacc_z)) + geom_boxplot()


#Single sensor/axis features only, 94 balanced and 90.5 F1 score
training_single <- training %>% select(code:ragyr_MAG)
rfmodel_single <- randomForest(code ~ ., data = training_single, localImp = TRUE, proximity = FALSE, ntree = 150)
get_metrics(rfmodel_single, testing)

#Left hip only, 90 balanced 84 F1
training_lh <- training %>% select(code, (contains("lhacc") | contains("lhgyr")) 
                    & !contains("rhacc") & !contains("rhgyr")
                    & !contains("laacc") & !contains("lagyr")
                    & !contains("raacc") & !contains("ragyr")) 
names(training_lh)
rfmodel_lh <- randomForest(code ~ ., data = training_lh, localImp = TRUE, proximity = FALSE, ntree = 150)
get_metrics(rfmodel_lh, testing)

#Right hip only, 91 balanced 86 F1
training_rh <- training %>% select(code, (contains("rhacc") | contains("rhgyr")) 
                                        & !contains("lhacc") & !contains("lhgyr")
                                        & !contains("laacc") & !contains("lagyr")
                                        & !contains("raacc") & !contains("ragyr")) 
rfmodel_rh <- randomForest(code ~ ., data = training_rh, localImp = TRUE, proximity = FALSE, ntree = 150)
get_metrics(rfmodel_rh, testing)

#Left ankle only, 89 balanced 82 F1
training_la <- training %>% select(code, (contains("laacc") | contains("lagyr")) 
                                   & !contains("lhacc") & !contains("lhgyr")
                                   & !contains("raacc") & !contains("ragyr")
                                   & !contains("rhacc") & !contains("rhgyr")) 
rfmodel_la <- randomForest(code ~ ., data = training_la, localImp = TRUE, proximity = FALSE, ntree = 150)
get_metrics(rfmodel_la, testing)

#Right ankle only, 89 balanced 82 F1
training_ra <- training %>% select(code, (contains("raacc") | contains("ragyr")) 
                                   & !contains("lhacc") & !contains("lhgyr")
                                   & !contains("laacc") & !contains("lagyr")
                                   & !contains("rhacc") & !contains("rhgyr")) 
rfmodel_ra <- randomForest(code ~ ., data = training_ra, localImp = TRUE, proximity = FALSE, ntree = 150)
get_metrics(rfmodel_ra, testing)

#Both hips, 94 balanced 89 F1 (basically same as full model)
training_hips <- training %>% select(code, (contains("lhacc") | contains("lhgyr") | contains("rhacc") | contains("rhgyr")) 
                                   & !contains("laacc") & !contains("lagyr") & !contains("raacc") & !contains("ragyr")) 
names(training_hips)
rfmodel_hips <- randomForest(code ~ ., data = training_hips, localImp = TRUE, proximity = FALSE, ntree = 150)
get_metrics(rfmodel_hips, testing)

min_depth_frame <- min_depth_distribution(rfmodel_hips)
plot_min_depth_distribution(min_depth_frame, k = 50)

#Both ankles, 91 balanced 85 F1 (worse than hips)
training_ankles <- training %>% select(code, (contains("laacc") | contains("lagyr") | contains("raacc") | contains("ragyr")) 
                                     & !contains("lhacc") & !contains("lhgyr") & !contains("rhacc") & !contains("rhgyr")) 
rfmodel_ankles <- randomForest(code ~ ., data = training_ankles, localImp = TRUE, proximity = FALSE, ntree = 150)
get_metrics(rfmodel_ankles, testing)
min_depth_frame <- min_depth_distribution(rfmodel_ankles)
plot_min_depth_distribution(min_depth_frame, k = 50)

# Only acceleration, 91% balanced an 85% F score
training_acc <- training %>% select(code, contains("acc"))
rfmodel_acc <- randomForest(code ~ ., data = training_acc, localImp = TRUE, proximity = FALSE, ntree = 150)
get_metrics(rfmodel_acc, testing)

# Only gyro is quite bad, 80% balanced and 69% F score
training_gyr <- training %>% select(code, contains("gyr"))
rfmodel_gyr <- randomForest(code ~ ., data = training_gyr, localImp = TRUE, proximity = FALSE, ntree = 150)
get_metrics(rfmodel_gyr, testing)


