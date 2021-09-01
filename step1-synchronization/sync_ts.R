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
source(here("step1-synchronization","motion_features.R"))

id <- 102
session <- 1
who <- "infant"
start_time <- "2021-07-12 12:00:00"
end_time <- "2021-07-12 20:00:00"

# READ DATA -----
read_infant_imu <- function(name) {
  name_long <- name
  name <- str_split_fixed(name, "_", n = 3) %>% as.list(.) %>%  set_names(c("side","part","signal"))
  file <- here(id,session, "imu", glue("{name$side}_{name$part}"),glue("{name$signal}.csv"))
  col_names <- c("time", 
                 glue("{str_sub(name$side,1,1)}{str_sub(name$part,1,1)}{str_sub(name$signal,1,3)}_x"), 
                 glue("{str_sub(name$side,1,1)}{str_sub(name$part,1,1)}{str_sub(name$signal,1,3)}_y"), 
                 glue("{str_sub(name$side,1,1)}{str_sub(name$part,1,1)}{str_sub(name$signal,1,3)}_z"))
  assign(name_long, read_csv(file, skip = 1, col_names = col_names), envir = .GlobalEnv)
}

read_parent_imu <- function(name) {
  file <- here(id,session, "imu", "caregiver",glue("{name}.csv"))
  col_names <- c("time", glue("{str_sub(name,1,1)}acc_x"), glue("{str_sub(name,1,1)}acc_y"), glue("{str_sub(name,1,1)}acc_z"),
                 glue("{str_sub(name,1,1)}gyr_x"), glue("{str_sub(name,1,1)}gyr_y"), glue("{str_sub(name,1,1)}gyr_z"))
  assign(name, read_csv(file, skip = 1, col_names = col_names), envir = .GlobalEnv)
}

if (who == "infant") {
  sensor_data <- c("left_ankle_accel", "right_ankle_accel", "left_hip_accel", "right_hip_accel", "left_ankle_gyro", "right_ankle_gyro", "left_hip_gyro","right_hip_gyro")
  walk(sensor_data, ~read_infant_imu(.x))
} else if (who == "parent") {
  sensor_data <- c("wrist", "hip")
  walk(sensor_data, ~read_parent_imu(.x))
}

# FIX TIMES ----
filter_and_fix_time <- function(data_string, start_time, end_time, who) {
  temp_ds <- get(data_string)
  fix_biostamp_time <- function(x) as_datetime((round(x/1000000, 2)), tz = "America/Los_Angeles")
  
  temp_ds <- temp_ds %>% mutate(time = fix_biostamp_time(time))
  if (who == "parent") {
    temp_ds <- temp_ds %>% mutate(time = time + hours(1))
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


# IMPORT CODED ACTIVITY -----
activity <- read_csv(here(id,session, "coding", "activity.csv"), col_names = c("onset", "offset", "code"))
activity <- activity %>% mutate(across(onset:offset, ~ .x/1000))
valid_codes <- c("d","u","s","sr","ss","sc","w","c","p","hs","hw","l")
activity_special <- activity %>% filter(!code %in% valid_codes)
activity <- activity %>% filter(code %in%valid_codes)

#Offset based on jump time

anno <- read_csv(here(id,session, "coding", "biostamp_annotations.csv")) %>% 
  rename_with(~ janitor::make_clean_names(.x)) %>% 
  mutate(across(start_timestamp_ms:stop_timestamp_ms, ~as_datetime((round(.x/1000, 2)), tz = "America/Los_Angeles")))

sync_point <- anno %>% filter(value == "sync") %>% pull(start_timestamp_ms)

activity  <-  activity %>% mutate(across(onset:offset, ~ sync_point + seconds(.x)))

# Match activity codes to imu data based on time
ds$code <- NA
for (i in 1:nrow(activity)) {
  ds[between_time(ds$time, activity$onset[i], activity$offset[i]),]$code <- activity$code[i]
} 

#Make ds subset that has entire coded period, not rest of day
start_time_coded <- activity %>% slice_head %>% pull(onset)
end_time_coded <- activity %>% slice_tail %>% pull(offset)
ds_coded <-ds %>% filter_by_time(time, start_time_coded, end_time_coded)

ds_coded %>% plot_time_series(time, laacc_x, .color_var = code, .smooth = F)


# MOTION FEATURES ------
# sliding 4 second windows every 1 second
slide <- slide_period_dfr(ds_coded, .i = ds_coded$time, .period = "second", .every = 2, .after = 1, ~ motion_features(.x, who))

save(slide, file = here(id,session, "synced_data", glue("mot_features_{who}.RData"))

#SHOULD ALSO INCLUDE A FEATURE SET FOR THE ENTIRE DAY...


# DEBUGGING ----------- 
#IF MORE CONTROL OVER TIME IS NEEDED, I CAN ADJUST SECONDS BY A MULTIPLIER

#DIVIDE TIME BY 4 TO MAKE EACH SECOND A 4 SECOND WINDOW
#ds$time_compressed <- as.POSIXct(as_date(seconds(ds$time)/4))

# THEN RUN THE SLIDER

#MULTIPLY TIME BY 4 TO RETURN TO ORIGINAL TIMESCALE
#slide$time <- as.POSIXct(as_date(seconds(slide$time)*4))
