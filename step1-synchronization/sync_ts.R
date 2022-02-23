library(timetk)
library(lubridate)
library(here)
library(janitor)
library(slider)
library(randomForest)
library(cvms)
library(caret)
library(tidyverse)
library(glue)
i_am(".here")
source(here("code","step1-synchronization","motion_features.R"))

id <- 104
session <- 1
who <- "infant"

# sync_info <- read_csv(here("data","sync_info.csv"))
# start_time <- sync_info %>% filter(id == .env[["id"]], session == .env[["session"]]) %>% pull(start_time)

start_time <- "2022-01-03 09:30:00"
end_time <- "2022-01-03 17:00:00"
complete <-  TRUE

session_param <- list(id = id, session = session, who = who, start_time = start_time, end_time = end_time, complete = complete)

# READ DATA -----
# Reads infant biostamp data 
# Most of this code is just trying to assign consistent names for each body part x direction x signal type
read_infant_imu <- function(name) {
  name_long <- name
  name <- str_split_fixed(name, "_", n = 3) %>% as.list(.) %>%  set_names(c("side","part","signal"))
  file <- here("data",id,session, "imu", glue("{name$side}_{name$part}"),glue("{name$signal}.csv"))
  col_names <- c("time", 
                 glue("{str_sub(name$side,1,1)}{str_sub(name$part,1,1)}{str_sub(name$signal,1,3)}_x"), 
                 glue("{str_sub(name$side,1,1)}{str_sub(name$part,1,1)}{str_sub(name$signal,1,3)}_y"), 
                 glue("{str_sub(name$side,1,1)}{str_sub(name$part,1,1)}{str_sub(name$signal,1,3)}_z"))
  assign(name_long, read_csv(file, skip = 1, col_names = col_names), envir = .GlobalEnv)
}

# This reads parent axivity data
read_parent_imu <- function(name) {
  file <- here("data",id,session, "imu", "caregiver",glue("{name}.csv"))
  col_names <- c("time", glue("{str_sub(name,1,1)}acc_x"), glue("{str_sub(name,1,1)}acc_y"), glue("{str_sub(name,1,1)}acc_z"),
                 glue("{str_sub(name,1,1)}gyr_x"), glue("{str_sub(name,1,1)}gyr_y"), glue("{str_sub(name,1,1)}gyr_z"))
  assign(name, read_csv(file, skip = 1, col_names = col_names), envir = .GlobalEnv)
}

# Read the data for each file
if (who == "infant") {
  sensor_data <- c("left_ankle_accel", "right_ankle_accel", "left_hip_accel", "right_hip_accel", "left_ankle_gyro", "right_ankle_gyro", "left_hip_gyro","right_hip_gyro")
  walk(sensor_data, ~read_infant_imu(.x))
} else if (who == "parent") {
  sensor_data <- c("wrist", "hip")
  walk(sensor_data, ~read_parent_imu(.x))
}

# FIX TIMES ----
# Get timestamps into participant local datetimes
filter_and_fix_time <- function(data_string, start_time, end_time, who) {
  temp_ds <- get(data_string)
  fix_biostamp_time <- function(x) as_datetime((round(x/1000000, 2)), tz = "America/Los_Angeles")
  
  if (who == "infant") {
    temp_ds <- temp_ds %>% mutate(time = fix_biostamp_time(time))
    } else 
  if (who == "parent") {
    temp_ds <- temp_ds %>% mutate(time = as_datetime(time, tz = "America/Los_Angeles") + hours(7))
    temp_ds <- temp_ds %>% mutate(time = time + hours(1)) #CRAP IS THIS A DAYLIGHT SAVINGS THING?
  }  
  temp_ds <- temp_ds %>% filter_by_time(time, start_time, end_time) 
  assign(paste0(data_string,"_filt"), temp_ds, envir = .GlobalEnv)
}

walk(sensor_data, ~ filter_and_fix_time(.x, start_time, end_time, who))
sdfilt <- map_chr(sensor_data, ~paste0(.x,"_filt"))

# JOIN SENSORS INTO SINGLE FRAME ----
ds <-  full_join(get(sdfilt[1]), get(sdfilt[2])) 
if (who == "infant") {
  for (i in 3:length(sdfilt)){
    ds <- full_join(ds, get(sdfilt[i]))
  }
}
ds <- ds %>% arrange(time)
ds  <- ds %>% mutate(across(-time, ~ts_impute_vec(.x)))

#ds %>% plot_time_series(time, laacc_x, .smooth = F, .interactive = F)

# USE INFANT SYNC POINT FROM BIOSTAMP TO CORRECT ACTIVITY TIMES -----
#FOR SOME PPTS, NEED TO ADD A CONSTANT OF MINUS 1 HOUR, PROBABLY A DST ISSUE

# This annotation file contains the detected sync point time from the time series
anno <- read_csv(here("data",id,session, "coding", "biostamp_annotations.csv")) %>% 
  rename_with(~ janitor::make_clean_names(.x)) %>% 
  mutate(across(start_timestamp_ms:stop_timestamp_ms, ~as_datetime((round(.x/1000, 2)), tz = "America/Los_Angeles"))) #- hours(1)

sync_point <- anno %>% filter(value == "sync") %>% pull(start_timestamp_ms)

# IMPORT CODED ACTIVITY FROM DATAVYU -----
if (who == "infant") {
  activity <- read_csv(here("data",id, session, "coding", "activity.csv"),col_names = c("onset", "offset", "code"))
} else
  if (who == "parent") {
    activity <- read_csv(here("data",id, session, "coding", "activity_parent.csv"),col_names = c("onset", "offset", "code"))
  }
activity <- activity %>% mutate(across(onset:offset, ~ sync_point + seconds(.x/1000)))
valid_codes <- c("d","u","s","sr","ss","sc","w","c","p","hs","hw","l")
activity_special <- activity %>% filter(!code %in% valid_codes)
activity <- activity %>% filter(code %in%valid_codes)

#NEED A BETTER WAY OF GETTING WRIST SYNC POINTS FROM THE DATA
if (who == "parent") {
  wrist_video_time <- activity_special %>% filter(code == "wrist") %>% pull(onset) 
  wrist_sync <- ds %>% filter(wacc_x > 3) %>% slice_head() %>%  pull(time) #NEED TO FIX THIS FOR FUTURE PPTS
  time_diff <- wrist_sync - wrist_video_time
  ds$time <- ds$time - time_diff
}

# Match activity codes to imu data based on time
ds$code <- as.character(NA)
for (i in 1:nrow(activity)) {
  ds[between_time(ds$time, activity$onset[i], activity$offset[i]),]$code <- activity$code[i]
} 

ds_coded <-ds %>% filter_by_time(time, start_time, end_time)

#Identify start and end times of coded period

start_time_coded <- activity %>% slice_head %>% pull(onset)
end_time_coded <- activity %>% slice_tail %>% pull(offset)

# ds_coded %>% plot_time_series(time, laacc_x, .color_var = code, .smooth = F)
# ds_coded %>% plot_time_series(time, hacc_z, .color_var = code, .smooth = F)

# MOTION FEATURES ------
# sliding 4 second windows every 1 second

#CHANGE COMPLETE TO TRUE TO GET ALL FEATURES

slide <- slide_period_dfr(ds, 
                          .i = ds$time, 
                          .period = "second", 
                          .every = 2, 
                          .after = 1, 
                          .origin = start_time_coded, 
                          .complete = TRUE,
                          ~ motion_features(.x, who, complete = complete))

#Note video and nap periods
slide$video_period <- 0
slide[between_time(slide$time, start_time_coded, end_time_coded),]$video_period <- 1

slide$nap_period <- 0
nap <- anno %>% filter(value == "nap")
for (i in 1:nrow(nap)) {
  slide[between_time(slide$time, nap$start_timestamp_ms[i], nap$stop_timestamp_ms[i]),]$nap_period <- 1
} 

slide$exclude_period <- 0
excl <- anno %>% filter(value == "exclude")
for (i in 1:nrow(excl)) {
  slide[between_time(slide$time, excl$start_timestamp_ms[i], excl$stop_timestamp_ms[i]),]$exclude_period <- 1
}

session_param$start_time_coded <- start_time_coded
session_param$end_time_coded <- end_time_coded

save(slide, session_param, file = here("data",id,session, "synced_data", glue("mot_features_{who}.RData")))


# DEBUGGING ----------- 
#IF MORE CONTROL OVER TIME IS NEEDED, I CAN ADJUST SECONDS BY A MULTIPLIER

#DIVIDE TIME BY 4 TO MAKE EACH SECOND A 4 SECOND WINDOW
#ds$time_compressed <- as.POSIXct(as_date(seconds(ds$time)/4))

# THEN RUN THE SLIDER

#MULTIPLY TIME BY 4 TO RETURN TO ORIGINAL TIMESCALE
#slide$time <- as.POSIXct(as_date(seconds(slide$time)*4))

#check that slide runs over entire period (it does)
# slide_test <- slide_period(ds, 
# .i = ds$time, 
# .period = "second", 
# .every = 2, 
# .after = 1, 
# .origin = start_time_coded, 
# ~ median(.x$time, na.rm = T))