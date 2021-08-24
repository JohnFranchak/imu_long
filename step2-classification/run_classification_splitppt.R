library(randomForest)
library(tidyverse)
library(caret)
library(cvms)
library(here)
source(here("step2-classification","collapse_classes.R"))
library(glue)
library(xtable)
library(lubridate)

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme_update(text = element_text(size = 16),
             axis.text.x = element_text(size = 16, color = "black"), axis.title.x = element_text(size = 21),
             axis.text.y = element_text(size = 16, color = "black"), axis.title.y = element_text(size = 21),
             panel.background = element_blank(),panel.border = element_blank(), panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), 
             axis.ticks.length = unit(.2, "cm"), axis.ticks = element_line(size = 1, lineend = "round"),
             legend.key = element_rect(fill = "white")) 

samples = 100
ntree = 150
mtry = NULL
outcome = "posture"
include_pressure = F
save = T
include_fft = T

set.seed(42)
options(readr.num_columns = 0)

all_class_num <- c(1,2,3,4,5,6,7,8,9,10)
all_class_lab <-c("upright", "walking", "prone", "crawling","held_walk","held_stat","sit_surf","sit_cg","sit_rest","supine")

fft_string <- ifelse(include_fft, "fft","nofft")
filename <- paste0(outcome, "_trees",ntree,"_samples",samples,"_",fft_string,"_split")

fname <- here("102","1","imu","classification100.txt")
ds_full <- read_csv(fname)
ds_full$time <- as.numeric(row.names(ds))
ds_full$clock_time_parsed <- parse_date_time(ds_full$clock_time, "%d-%B-%y %H:%M:%S") - hours(7)

if (include_fft == F) {
  ds_full <- select(ds_full, !contains("fft"))
}

ds_full$classe <- ds_full$class

#DROP NOT-FULL CASES, DISAGREEMENTS
ds <- ds_full %>% filter(video_time == 0, class_prop > .5) %>% select(-video_time, -nap_time)

#SPLIT INTO TRAINING TESTING, THEN RECLASSIFY AND DROP CLASSES
ds <- ds %>% arrange(classe, time)
training <- ds %>% group_by(classe) %>% slice_head(prop = .65) %>% ungroup %>% select(-(1:4))
testing <- ds %>% group_by(classe) %>% slice_tail(prop = .35) %>% ungroup %>% select(-(1:4))

training <- collapse_classes(outcome, training)
training$classe <- fct_drop(training$classe)

testing <- collapse_classes(outcome, testing)
testing$classe <- fct_drop(testing$classe)

#RF MODEL
rfmodel <- randomForest(classe ~ ., data = training, localImp = TRUE, proximity = FALSE,na.action=na.roughfix, ntree = ntree)
predictions <- predict(rfmodel, testing, type = "class")

u <- union(predictions, testing$classe)
res <- confusion_matrix(factor(testing$classe, u),factor(predictions, u))

res$`Overall Accuracy`
res$`Balanced Accuracy`
res$Kappa
plot_confusion_matrix(res$`Confusion Matrix`[[1]])

#save(list = c("res","rfmodel","filename","testing","training"),file=here(paste0(filename,".RData")))

##ALL VIDEO TRAINING MODEL
training_all <- ds %>% select(-(1:4))
training_all <- collapse_classes(outcome, training_all)
training_all$classe <- fct_drop(training_all$classe)

rfmodel_all <- randomForest(classe ~ ., data = training_all, localImp = TRUE, proximity = FALSE,na.action=na.roughfix, ntree = ntree)

full_day <- ds_full %>% filter(video_time == 1 | video_time == 0) %>% select(-video_time)
full_day_times <- select(full_day, time, clock_time, nap_time) 
full_day <- full_day %>% select(-(1:3)) %>% select(-nap_time)

#Predict from all
predictions_full <- predict(rfmodel_all, full_day, type = "class")
p <- bind_cols(full_day_times, predictions_full) %>% set_names(c("time","clock_time","nap","posture"))

#Predict from training data
predictions_part <- predict(rfmodel, full_day, type = "class")
p <- bind_cols(p, posture_part = predictions_part)

nap_on_off <- p %>% filter(nap == 0) %>% summarize(nap_on = min(time), nap_off = max(time))

ggplot(p) + 
  geom_path(aes(x = time, y = 1,color = posture, group = 1L), size = 20) + 
  #geom_path(aes(x = time, y = .995 ,color = posture_part, group = 1L), size = 6) +
  ylim(.95,1.05) + theme(legend.position = "top") + 
  geom_vline(xintercept = c(nap_on_off$nap_on, nap_on_off$nap_off)) 

ggplot(p) + 
  geom_path(aes(x = as.POSIXct(clock_time), y = 1,color = posture, group = 1L), size = 20) + 
  scale_x_datetime(labels ="%H:%M") + 
  ylim(.95,1.05) + theme(legend.position = "top") 

p_awake <- p %>% filter(nap == 1)
left_join(fct_count(p_awake$posture, prop = T), fct_count(p_awake$posture_part, prop = T), by = "f")

save(file = "model_data/model_data.RData", list = c("p", "rfmodel","rfmodel_all","nap_on_off"))


