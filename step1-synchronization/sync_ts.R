library(timetk)
library(data.table)
library(here)
library(janitor)
library(slider)
library(tidyverse)
i_am(".here")
source(here("code","step1-synchronization","motion_features.R"))

id <- 130
session <- 2
who <- "infant"

# start_time <- "2022-09-01 01:15:00"
# end_time <- "2022-09-01 21:15:00"

ppt_list <- read_csv(here("data","ppt_info.csv")) %>% 
  filter(id == .env[["id"]], session == .env[["session"]])
start_time <- paste(mdy(ppt_list$date), ppt_list$start_time)
end_time <- paste(mdy(ppt_list$date), ppt_list$end_time)

complete <-  TRUE

session_param <- list(id = id, session = session, who = who, start_time = start_time, end_time = end_time, complete = complete)

# READ DATA -----
imu_column <- function(name) {
  name <- str_split_fixed(name, "_", n = 3) %>% as.list(.) %>%  set_names(c("side","part","signal"))
  col_names <- c("time", 
                 str_glue("{str_sub(name$side,1,1)}{str_sub(name$part,1,1)}{str_sub(name$signal,1,3)}_x"), 
                 str_glue("{str_sub(name$side,1,1)}{str_sub(name$part,1,1)}{str_sub(name$signal,1,3)}_y"), 
                 str_glue("{str_sub(name$side,1,1)}{str_sub(name$part,1,1)}{str_sub(name$signal,1,3)}_z"))
}
imu_file <- function(name, id, session) {
  name <- str_split_fixed(name, "_", n = 3) %>% as.list(.) %>%  set_names(c("side","part","signal"))
  file <- here("data",id,session, "imu", str_glue("{name$side}_{name$part}"),str_glue("{name$signal}.csv"))
}

sensor_data <- c("left_ankle_accel", "right_ankle_accel", "left_hip_accel", "right_hip_accel", "left_ankle_gyro", "right_ankle_gyro", "left_hip_gyro","right_hip_gyro")
read_imu <- function(name, id, session) {fread(imu_file(name, id, session), skip = 1, col.names = imu_column(name), sep = ',', verbose = T)}
read_imu(sensor_data[1], id, session)

map(sensor_data, ~fread(imu_file(.x, id, session), skip = 1, col.names = imu_column(.x), sep = ',', verbose = T))

# FIX TIMES ----
# Get timestamps into participant local datetimes
filter_and_fix_time <- function(data_string, start_time, end_time) {
  temp_ds <- get(data_string)
  fix_biostamp_time <- function(x) as_datetime((round(x/1000000, 2)), tz = "America/Los_Angeles")
  temp_ds <- temp_ds %>% mutate(time = fix_biostamp_time(time))
  temp_ds <- temp_ds %>% filter_by_time(time, start_time, end_time) 
  assign(paste0(data_string,"_filt"), temp_ds, envir = .GlobalEnv)
}

walk(sensor_data, ~ filter_and_fix_time(.x, start_time, end_time))
sdfilt <- map_chr(sensor_data, ~paste0(.x,"_filt"))

# JOIN SENSORS INTO SINGLE FRAME ----
ds <-  full_join(get(sdfilt[1]), get(sdfilt[2])) 
for (i in 3:length(sdfilt)){
  ds <- full_join(ds, get(sdfilt[i]))
}
ds <- ds %>% arrange(time)
ds  <- ds %>% mutate(across(-time, ~ts_impute_vec(.x)))

# USE INFANT SYNC POINT FROM BIOSTAMP TO CORRECT ACTIVITY TIMES -----

# This annotation file contains the detected sync point time from the time series
anno <- read_csv(here("data",id,session, "coding", "biostamp_annotations.csv")) %>% 
  rename_with(~ janitor::make_clean_names(.x)) %>% 
  mutate(across(start_timestamp_ms:stop_timestamp_ms, ~as_datetime((round(.x/1000, 2)), tz = "America/Los_Angeles")))

anno <- anno %>% filter_by_time(start_timestamp_ms, as_datetime(start_time) - hours(4), end_time)
sync_point <- anno %>% filter(value == "sync") %>% pull(start_timestamp_ms)

# IMPORT CODED ACTIVITY FROM DATAVYU -----
activity <- read_csv(here("data",id, session, "coding", "activity.csv"),col_names = c("onset", "offset", "code"))
activity <- activity %>% mutate(across(onset:offset, ~ sync_point + seconds(.x/1000)))
valid_codes <- c("d","u","s","sr","ss","sc","w","c","p","hs","hw","l")
activity_special <- activity %>% filter(!code %in% valid_codes)
activity <- activity %>% filter(code %in%valid_codes)

# Match activity codes to imu data based on time
ds$code <- as.character(NA)
for (i in 1:nrow(activity)) {
  ds[between_time(ds$time, activity$onset[i], activity$offset[i]),]$code <- activity$code[i]
} 

#Identify start and end times of coded period

start_time_coded <- activity %>% slice_head %>% pull(onset)
end_time_coded <- activity %>% slice_tail %>% pull(offset)

# MOTION FEATURES ------
# sliding 4 second windows every 1 second

#CHANGE COMPLETE TO TRUE TO GET ALL FEATURES
# FOR TESTING INDIVIDUAL WINDOWS
# ds_t <- ds %>% filter_by_time(time, "2022-04-09 17:47:33", "2022-04-09 17:47:36") # to get a single window

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
if ("nap" %in% unique(anno$value)) {
  nap <- anno %>% filter(value == "nap")
  for (i in 1:nrow(nap)) {
    slide[between_time(slide$time, nap$start_timestamp_ms[i], nap$stop_timestamp_ms[i]),]$nap_period <- 1
  } 
}

slide$exclude_period <- 0
if ("exclude" %in% unique(anno$value)) {
  excl <- anno %>% filter(value == "exclude")
  for (i in 1:nrow(excl)) {
    slide[between_time(slide$time, excl$start_timestamp_ms[i], excl$stop_timestamp_ms[i]),]$exclude_period <- 1
  }
}

session_param$start_time_coded <- start_time_coded
session_param$end_time_coded <- end_time_coded

save(slide, session_param, file = here("data",id,session, "synced_data", str_glue("mot_features_{who}.RData")))

source(here("code","step2-classification","run_classification_split.R"))
run_classification_split(id = id, session = session, type = "split")
run_classification_split(id = id, session = session, type = "group")

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