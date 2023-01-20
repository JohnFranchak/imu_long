
require(here)
require(randomForest)
require(cvms)
require(caret)
require(tidyverse)
require(glue)
i_am(".here")

#LOAD DATA
load( here("code","tune_ml","compiled_data_lite.RData"))
load( here("tune_ml","compiled_data_lite.RData"))

training <- slide_filt %>% group_by(code) %>% slice_head(prop = .75) %>% ungroup %>% select(-id)
testing <- slide_filt %>% group_by(code) %>% slice_tail(prop = .25) %>% ungroup %>% select(-id)

not_all_na <- function(x) any(!is.na(x))
training <- training %>% select_if(not_all_na)
rfmodel <- randomForest(code ~ ., data = training, localImp = TRUE, proximity = FALSE, ntree = 150)

predictions <- predict(rfmodel, testing, type = "class")
u <- union(predictions, testing$code)
res <- confusion_matrix(factor(testing$code, u),factor(predictions, u))
print(res$`Balanced Accuracy`)
print(res$`Table`)

# 
# save(rfmodel, file =  here("code","tune_ml","group_model.RData"))
