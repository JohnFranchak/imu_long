run_classification_julia <- function(id, session, who = "infant", type = "split") {

require(lubridate)
require(here)
require(randomForest)
require(cvms)
require(caret)
require(tidyverse)
require(glue)
i_am(".here")

#LOAD DATA
 id <- 11
 session <- 1
 who <- "infant"
 type <- "split"

 slide <- read_csv(here("data",id,session, "synced_data", str_glue("mot_features_{who}.csv")))
 code <- read_csv(here("data",id,session, "synced_data", str_glue("codes.csv")))
 
 slide_filt <- left_join(code, slide, by = "time_start")


slide_filt <- slide_filt %>% select(-n, -total, -(start_time_coded:video_period))

slide_filt$code = ifelse(slide_filt$code == "sr", "ss", slide_filt$code)
slide_filt$code <- factor(slide_filt$code, levels = c("hs", "l","p","ss","u"), labels = c("Held", "Supine","Prone","Sitting","Upright"))

slide_filt <- slide_filt %>% filter(prop > .75) %>% drop_na(code) %>% select(-prop)

#SPLIT INTO TRAINING TESTING, THEN RECLASSIFY AND DROP CLASSES
slide_filt <- slide_filt %>% arrange(code, time_start)

training <- slide_filt %>% group_by(code) %>% slice_head(prop = .6) %>% ungroup %>% select(-time_start) 
testing <- slide_filt %>% group_by(code) %>% slice_tail(prop = .4) %>% ungroup %>% select(-time_start) 

not_all_na <- function(x) any(!is.na(x))
training <- training %>% select_if(not_all_na)

if (type == "split") {
  rfmodel <- randomForest(code ~ ., data = training, localImp = TRUE, proximity = FALSE, ntree = 550, mtry = 44)
  predictions <- predict(rfmodel, testing, type = "class")
  u <- union(predictions, testing$code)
  res <- confusion_matrix(factor(testing$code, u),factor(predictions, u))
}
if (type == "group") {
  load(here("code","tune_ml","group_model.RData"))
  predictions <- predict(rfmodel, slide_filt, type = "class")
  u <- union(predictions, slide_filt$code)
  res <- confusion_matrix(factor(slide_filt$code, u),factor(predictions, u))
}

print(res$`Balanced Accuracy`)
print(res$`Table`)
print(res$`Kappa`)

#save(res, file =  here("data",id,session, "synced_data", glue("model_performance_{who}_{type}.RData")))

#write_csv(predictions_full, here("data",id,session, "synced_data", glue("position_predictions_{who}_{type}.csv")))

}
