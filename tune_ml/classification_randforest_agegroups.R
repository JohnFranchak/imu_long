
library(here)
library(randomForest)
library(cvms)
library(caret)
library(tidyverse)
library(glue)
not_all_na <- function(x) any(!is.na(x))

#LOAD DATA
load( here("tune_ml","compiled_data_younger.RData"))
slide_younger <- slide_filt
rm(slide_filt)

load( here("tune_ml","compiled_data_older.RData"))
slide_older <- slide_filt
rm(slide_filt)
rm(session)

slide_all <- bind_rows(slide_younger, slide_older)

training_all <- slide_all %>% group_by(code) %>% slice_head(prop = .75) %>% ungroup %>% select(-id) %>% select_if(not_all_na)
training_older <- slide_older %>% group_by(code) %>% slice_head(prop = .75) %>% ungroup %>% select(-id) %>% select_if(not_all_na)
training_younger <- slide_younger %>% group_by(code) %>% slice_head(prop = .75) %>% ungroup %>% select(-id) %>% select_if(not_all_na)
names_to_keep <- intersect(names(training_all), names(training_younger)) %>% intersect(names(training_older))

training_all <- training_all %>% select(all_of(names_to_keep))
training_older <- training_older %>% select(all_of(names_to_keep))
training_younger <- training_younger %>% select(all_of(names_to_keep))

testing_all <- slide_all %>% group_by(code) %>% slice_tail(prop = .25) %>% ungroup %>% select(-id) %>% select(all_of(names_to_keep))
testing_older <- slide_older %>% group_by(code) %>% slice_tail(prop = .25) %>% ungroup %>% select(-id) %>% select(all_of(names_to_keep))
testing_younger <- slide_younger %>% group_by(code) %>% slice_tail(prop = .25) %>% ungroup %>% select(-id) %>% select(all_of(names_to_keep))

rfmodel_younger <- randomForest(code ~ ., data = training_younger, localImp = TRUE, proximity = FALSE, ntree = 150)
rfmodel_older <- randomForest(code ~ ., data = training_older, localImp = TRUE, proximity = FALSE, ntree = 150)
rfmodel_all <- randomForest(code ~ ., data = training_all, localImp = TRUE, proximity = FALSE, ntree = 150)

acc <- function(rfmodel, testing_data) {
  predictions <- predict(rfmodel, testing_data, type = "class")
  u <- union(predictions, testing_data$code)
  res <- confusion_matrix(factor(testing_data$code, u),factor(predictions, u))
  print(res$`Balanced Accuracy`)
  return(res)
}

#WITHIN
acc_younger_from_younger <- acc(rfmodel_younger, testing_younger)
acc_older_from_older <- acc(rfmodel_older, testing_older)
acc_all_from_all <- acc(rfmodel_all, testing_all)

#BETWEEN
acc_older_from_younger <- acc(rfmodel_younger, testing_older)
acc_younger_from_older <- acc(rfmodel_older, testing_younger)

#ALL FROM SINGLE GROUP
acc_all_from_younger <- acc(rfmodel_younger, testing_all)
acc_all_from_older <- acc(rfmodel_older, testing_all)

#SINGLE GROUP FROM ALL MODEL
acc_older_from_all <- acc(rfmodel_all, testing_older)
acc_younger_from_all <- acc(rfmodel_all, testing_younger)




# 
# save(rfmodel, file =  here("code","tune_ml","group_model.RData"))
