run_classification_split <- function(id, session, who = "infant", type = "split") {

require(timetk)
require(lubridate)
require(here)
require(janitor)
require(slider)
require(psych)
require(corrr)
require(randomForest)
require(cvms)
require(caret)
require(tidyverse)
require(glue)
i_am(".here")

#LOAD DATA
# id <- 110
# session <- 4
# who <- "infant"
# type <- "group"
load(here("data",id,session, "synced_data", str_glue("mot_features_{who}.RData")))

print(str_glue("Running id {id} session {session}"))

slide_filt <- slide %>% filter(video_period == 1) %>% select(-video_period, -nap_period)
#CODE FACTORS
if (who == "parent") {
  slide_filt$code = ifelse(slide_filt$code == "s", "d", slide_filt$code)
  slide_filt$code <- factor(slide_filt$code, levels = c("u", "d"), labels = c("Upright", "Down"))
  # slide_filt$code <- factor(slide_filt$code, levels = c("u", "d", "s"), labels = c("Upright", "Down", "Sitting")) 
} else if (who == "infant") {
  slide_filt$code = ifelse(slide_filt$code == "sr", "ss", slide_filt$code)
  slide_filt$code <- factor(slide_filt$code, levels = c("hs", "l","p","ss","u"), labels = c("Held", "Supine","Prone","Sitting","Upright"))
}
slide_filt <- slide_filt %>% filter(code_prop > .75) %>% drop_na(code) %>% select(-code_prop)

#SPLIT INTO TRAINING TESTING, THEN RECLASSIFY AND DROP CLASSES
slide_filt <- slide_filt %>% arrange(code, time)

training <- slide_filt %>% group_by(code) %>% slice_head(prop = .6) %>% ungroup %>% select(-time) 
testing <- slide_filt %>% group_by(code) %>% slice_tail(prop = .4) %>% ungroup %>% select(-time) 

not_all_na <- function(x) any(!is.na(x))
training <- training %>% select_if(not_all_na)

if (type == "split") {
  rfmodel <- randomForest(code ~ ., data = training, localImp = TRUE, proximity = FALSE, ntree = 150)
  predictions <- predict(rfmodel, testing, type = "class")
  u <- union(predictions, testing$code)
  res <- confusion_matrix(factor(testing$code, u),factor(predictions, u))
}
if (type == "group") {
  load("tune_ml/group_model.RData")
  predictions <- predict(rfmodel, slide_filt, type = "class")
  u <- union(predictions, slide_filt$code)
  res <- confusion_matrix(factor(slide_filt$code, u),factor(predictions, u))
}

print(res$`Balanced Accuracy`)
print(res$`Table`)

save(res, file =  here("data",id,session, "synced_data", glue("model_performance_{who}_{type}.RData")))

#FULL DAY ---- 
if (is.null(slide$exclude_period)) slide$exclude_period <- 0


predictions_full <- slide %>% 
  select(time, nap_period, video_period, exclude_period) %>% 
  bind_cols(tibble(pos = predict(rfmodel, slide, type = "class"))) %>% 
  filter(exclude_period == 0) 



predictions_full %>% filter(nap_period == 0) %>% pull(pos) %>% fct_count(prop = T)

write_csv(predictions_full, here("data",id,session, "synced_data", glue("position_predictions_{who}_{type}.csv")))

}
