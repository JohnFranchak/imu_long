library(timetk)
library(lubridate)
library(here)
library(janitor)
library(slider)
library(randomForest)
library(cvms)
library(caret)
library(tidyverse)
source(here("step1-synchronization","motion_features.R"))

id <- 102
session <- 1


#READ IN DATA
lankle_acc <- read_csv(here(id,session, "imu", "left_ankle","accel.csv"), skip = 1,  col_names = c("time", "laacc_x", "laacc_y", "laacc_z")) 
rankle_acc <- read_csv(here(id,session, "imu", "right_ankle","accel.csv"), skip = 1, col_names = c("time", "raacc_x", "raacc_y", "raacc_z")) 
lhip_acc <- read_csv(here(id,session, "imu", "left_hip","accel.csv"), skip = 1, col_names = c("time", "lhacc_x", "lhacc_y", "lhacc_z")) 
rhip_acc <- read_csv(here(id,session, "imu", "right_hip","accel.csv"), skip = 1, col_names = c("time", "rhacc_x", "rhacc_y", "rhacc_z")) 
lankle_gyr <- read_csv(here(id,session, "imu", "left_ankle","gyro.csv"), skip = 1,  col_names = c("time", "lagyr_x", "lagyr_y", "lagyr_z")) 
rankle_gyr <- read_csv(here(id,session, "imu", "right_ankle","gyro.csv"), skip = 1, col_names = c("time", "ragyr_x", "ragyr_y", "ragyr_z")) 
lhip_gyr <- read_csv(here(id,session, "imu", "left_hip","gyro.csv"), skip = 1, col_names = c("time", "lhgyr_x", "lhgyr_y", "lhgyr_z")) 
rhip_gyr <- read_csv(here(id,session, "imu", "right_hip","gyro.csv"), skip = 1, col_names = c("time", "rhgyr_x", "rhgyr_y", "rhgyr_z")) 

fix_biostamp_time <- function(x) as_datetime((round(x/1000000, 2)), tz = "America/Los_Angeles")

ds1 <- lankle_acc %>% mutate(time = fix_biostamp_time(time))
ds2 <- rankle_acc %>% mutate(time = fix_biostamp_time(time))
ds <- full_join(ds1, ds2) %>% arrange(time)

ds <- full_join(lankle_acc %>% mutate(time = fix_biostamp_time(time)), rankle_acc %>% mutate(time = fix_biostamp_time(time)), by = c("time" = "time"))
ds <- full_join(ds, lhip_acc  %>% mutate(time = fix_biostamp_time(time)), by = c("time" = "time"))
ds <- full_join(ds, rhip_acc %>% mutate(time = fix_biostamp_time(time)), by = c("time" = "time"))
ds <- full_join(ds, lankle_gyr %>% mutate(time = fix_biostamp_time(time)), by = c("time" = "time"))
ds <- full_join(ds, rankle_gyr %>% mutate(time = fix_biostamp_time(time)), by = c("time" = "time"))
ds <- full_join(ds, lankle_gyr %>% mutate(time = fix_biostamp_time(time)), by = c("time" = "time"))
ds <- full_join(ds, rankle_gyr %>% mutate(time = fix_biostamp_time(time)), by = c("time" = "time"))
ds <- ds %>% arrange(time)


start_time <- "2021-07-12 12:20:00"
end_time <- "2021-07-12 12:40:00"

wrist_per <- wrist %>% filter_by_time(time, start_time, end_time)
hip_per <- hip %>% filter_by_time(time, start_time, end_time)

ds <- full_join(wrist_per, hip_per, by = c("time" = "time"))

ds %>% plot_time_series(time, wacc_x, .smooth = F, .interactive = F)
ds %>% plot_time_series(time, hacc_x, .smooth = F, .interactive = F)

ds_long <- ds %>% select(time, wacc_x, hacc_x) %>% 
  pivot_longer(wacc_x:hacc_x, names_to = "sensors", values_to = "acc")

ds_long %>% plot_time_series(time, acc, .facet_vars = "sensors",.smooth = F, .interactive = T, .facet_scales = "fixed")


#IMPORT ACTIVITY AND SCALE TIMES
activity <- read_csv(here("data","activity_parent.csv"), col_names = c("onset", "offset", "code"))
activity <- activity %>% mutate(across(onset:offset, ~ .x/1000))
activity_special <- activity %>% filter(!code %in% c("d","u","s"))
activity <- activity %>% filter(code %in% c("d","u","s"))

#Align to wrist codes
wrist_video_sync <- activity_special %>% filter(code == "wrist") %>% pull(onset)
activity  <-  activity %>% mutate(across(onset:offset, ~ .x - wrist_video_sync))

wrist_sync <- ds %>% filter(wacc_x > 3) %>% pull(time)
activity  <-  activity %>% mutate(across(onset:offset, ~ wrist_sync + seconds(.x)))

#Make longer ds subset that has entire coded period
start_time <- activity %>% slice_head %>% pull(onset)
end_time <- activity %>% slice_tail %>% pull(offset)

wrist_per <- wrist %>% filter_by_time(time, start_time, end_time)
hip_per <- hip %>% filter_by_time(time, start_time, end_time)

#SMOOTH MOTION DATA?

ds <- full_join(wrist_per, hip_per, by = c("time" = "time"))
ds <- ds %>% arrange(time)

ds$code <- NA
for (i in 1:nrow(activity)) {
  ds[between_time(ds$time, activity$onset[i], activity$offset[i]),]$code <- activity$code[i]
} 


ds %>% plot_time_series(time, hacc_x, .color_var = code, .smooth = F)


#BY MINUTES FOR DEBUGGING
# slide <- slide_period_dfr(ds, .i = ds$time, .period = "minute", .every = 2, .after = 1, ~ motion_features(.x, "parent"))

#ACTUAL ONE (sliding 4 second windows every 2 seconds)
slide <- slide_period_dfr(ds, .i = ds$time, .period = "second", .every = 2, .after = 1, ~ motion_features(.x, "parent"))

save(slide, file = "slide.RData")



#-----------
#IF MORE CONTROL OVER TIME IS NEEDED, I CAN ADJUST SECONDS BY A MULTIPLIER

#DIVIDE TIME BY 4 TO MAKE EACH SECOND A 4 SECOND WINDOW
#ds$time_compressed <- as.POSIXct(as_date(seconds(ds$time)/4))

# THEN RUN THE SLIDER

#MULTIPLY TIME BY 4 TO RETURN TO ORIGINAL TIMESCALE
#slide$time <- as.POSIXct(as_date(seconds(slide$time)*4))
#------------