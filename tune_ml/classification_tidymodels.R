library(mlr3verse)
library(mlr3extralearners)
library(paradox)
library(caret)
library(tidyselect)
library(here)
library(janitor)
library(cvms)
library(caret)
library(tidyverse)
library(glue)
# remotes::install_github("mlr-org/mlr3extralearners")

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
slide_filt <- slide_filt %>% filter(code_prop > .75) %>% drop_na(code) %>% select(-code_prop, -(time:time_to))
not_all_na <- function(x) !any(is.na(x))
slide_filt <- slide_filt %>% select_if(not_all_na)

task <- TaskClassif$new("Posture", backend = slide_filt, target = "code")
task$col_roles$group = "id" 
task$col_roles$feature = setdiff(task$col_roles$feature, "id")

future::plan("multicore")

# learner_forest = lrn("classif.ranger", importance = "permutation")
learner_forest = lrn("classif.randomForest")

learner_forest$param_set$values = list(mtry = 5, num.trees = 250)

# train/predict on all data (kinda useless since test == training)
learner_forest$train(task)
prediction <- learner_forest$predict(task)
prediction$confusion
measure <- msr("classif.acc")
prediction$score(measure)

#LOOCV
rcv = rsmp("loo")
rcv$instantiate(task)
rcv$instance #so i know which id was the test for each fold
time = Sys.time()
rr <- resample(task, learner_forest, rcv)
Sys.time() - time
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


