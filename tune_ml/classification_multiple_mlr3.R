library(mlr3verse)
library(paradox)
library(caret)
library(tidyselect)
library(here)
library(janitor)
library(cvms)
library(caret)
library(tidyverse)
library(glue)

#LOAD DATA
rds <- list.files(here("tune_ml","mot_features"), pattern = ".RData", full.names = T)

load(rds[1])
slide_all <- slide %>% 
  mutate(id = session_param$id*100 + session_param$session) %>% 
  filter(F)
session <- as_tibble(session_param) %>% filter(F)

for (r in rds) {
  load(r)

  slide_all <- slide %>% 
    mutate(id = session_param$id*100 + session_param$session) %>% 
    bind_rows(slide_all)
  session <- session %>% bind_rows(as_tibble(session_param))
  
  rm(slide)
  rm(session_param)
}


slide_filt <- slide_all %>% filter(video_period == 1) %>% select(-video_period, -nap_period)

#CODE FACTORS
slide_filt$code = ifelse(slide_filt$code == "sr", "ss", slide_filt$code)
slide_filt$code <- factor(slide_filt$code, levels = c("hs", "l","p","ss","u"), labels = c("Held", "Supine","Prone","Sitting","Upright"))
slide_filt <- slide_filt %>% filter(code_prop > .75) %>% drop_na(code) %>% select(-code_prop)




#SPLIT INTO TRAINING TESTING, THEN RECLASSIFY AND DROP CLASSES
slide_filt <- slide_filt %>% arrange(code, time)

training <- slide_filt %>% group_by(code) %>% slice_head(prop = .6) %>% ungroup %>% select(-time) 
testing <- slide_filt %>% group_by(code) %>% slice_tail(prop = .4) %>% ungroup %>% select(-time) 

not_all_na <- function(x) any(!is.na(x))
training <- training %>% select_if(not_all_na)
rfmodel <- randomForest(code ~ ., data = training, localImp = TRUE, proximity = FALSE, ntree = 150)

predictions <- predict(rfmodel, testing, type = "class")

u <- union(predictions, testing$code)
res <- confusion_matrix(factor(testing$code, u),factor(predictions, u))
res$`Balanced Accuracy`
res$`Table`

#future::plan("multiprocess")

set.seed(42)
options(readr.num_columns = 0)
setwd("~/Dropbox/imu_classification/")

ppts <- read_csv('ppt_sync.csv')
id_list <- ppts$id[ppts$include > -1] 
samples = 200

all_class_num <- c(1,2,3,4,5,6,7,8,9,10)
all_class_lab <-c("upright", "walking", "prone", "crawling","held_walk","held_stat","sit_surf","sit_cg","sit_rest","supine")

first_set <- TRUE

for (id in id_list) {
  fname <- paste(toString(id),"/classification",toString(samples),".txt",sep = "")
  ds <- read_csv(fname)
  ds$id <- id
  ds  <-  select(ds,!contains("p1") & !contains("p2") & !contains("p3")) #Drop pressures data
  # ds  <-  select(ds,!contains("a_sum") & !contains("1")) #Drop ankle data
  # ds  <-  select(ds,!contains("t_sum") & !contains("2")) #Drop thigh data
  # ds  <-  select(ds,!contains("h_sum") & !contains("3")) #Drop hip data
  ds$classe <- ds$class
  
  #POSTURE (5 categories)
  ds$classe[ds$classe %in% c(1,2)] <- 0 #Classify all upright together
  ds$classe[ds$classe %in% c(3,4)] <- 1 #Classify all prone together
  ds$classe[ds$classe %in% c(7,8,9)] <- 2 #Classify all sitting together
  ds$classe[ds$classe %in% c(5,6)] <- 3 #Classify all holding together
  ds$classe[ds$classe %in% c(10)] <- 4 #Classify all supine together
  ds$classe <- factor(ds$classe, levels = c(0,1,2,3,4), labels = c("upright","prone","sitting","held","supine"))
  ds = subset(ds, select = -c(1,2,3) )
  
  if (first_set) {
      training <- ds
      first_set <- FALSE
   } else {
      training <- rbind(training, ds)
  }
}

task <- TaskClassif$new("Posture", backend = training, target = "classe")
task$col_roles$group = "id" 
task$col_roles$feature = setdiff(task$col_roles$feature, "id")

learner_forest = lrn("classif.ranger", importance = "permutation")

learner_forest$train(task)
learner_forest$param_set$values = list(mtry = 5, num.trees = 749)

# predict data (kinda useless since test == training)
prediction <- learner_forest$predict(task)
prediction$confusion
measure <- msr("classif.acc")
prediction$score(measure)

#LOCV
rcv = rsmp("cv", folds = 16)
rcv$instantiate(task)
rcv$instance #so i know which id was the test for each fold
#time = Sys.time()
rr <- resample(task, learner_forest, rcv)
#Sys.time() - time
rr$score(measure)
rr$aggregate(measure)

bmr1 = as_benchmark_result(rr)

learner_xg <- lrn("classif.xgboost")
learner_xg$param_set$values = list(nrounds = 122, eta = 0.3396089, max_depth = 145, subsample = .614086, colsample_bytree = .704387)
rr_xg <- resample(task, learner_xg, rcv)
rr_xg$score(measure)
rr_xg$aggregate(measure)

bmr2 = as_benchmark_result(rr_xg)
bmr1$combine(bmr2)
autoplot(bmr1)

summary(learner_forest$model)

#Tuning RF
learner_forest$param_set
tune_ps = ParamSet$new(list(
  ParamInt$new("mtry", lower = 3, upper = 8),
  ParamInt$new("num.trees", lower = 200, upper = 1000)
))
evals20 = trm("evals", n_evals = 20)

rcv5 = rsmp("cv", folds = 5)

instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = learner_forest,
  resampling = rcv5,
  measure = measure,
  search_space = tune_ps,
  terminator = evals20
)

tuner = tnr("random_search")
tuner$optimize(instance)
instance$result_learner_param_vals  #mtry 5,  ntrees = 749
instance$result_y
instance$archive$data()


#Tuning XG
learner_xg$param_set
tune_ps = ParamSet$new(list(
  ParamDbl$new("eta", lower = 0, upper = .5),
  ParamInt$new("max_depth", lower = 5, upper = 200),
  ParamInt$new("nrounds", lower = 50, upper = 150),
  ParamDbl$new("subsample", lower = .5, upper = 1),
  ParamDbl$new("colsample_bytree", lower = .5, upper = 1)
))
evals200 = trm("evals", n_evals = 200)

rcv5 = rsmp("cv", folds = 5)

instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = learner_xg,
  resampling = rcv5,
  measure = measure,
  search_space = tune_ps,
  terminator = evals200
)

tuner = tnr("random_search")
tuner$optimize(instance)
instance$result_learner_param_vals
instance$result_y 
instance$archive$data()


