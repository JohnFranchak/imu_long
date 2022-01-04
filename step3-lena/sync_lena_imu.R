rm(list = ls())

library(tidyverse)
library(here)
library(lubridate)
library(glue)
i_am(".here")

id <- 102
session <- 4
who <- "infant"

p <- read_csv(here("data",id,session,"synced_data","position_predictions_infant.csv")) %>% 
  mutate(pos = factor(pos, levels = c("Prone","Upright","Held","Sitting","Supine")))
p$time <- with_tz(p$time, "America/Los_Angeles")
start_time <- min(p$time)
stop_time <- max(p$time)

lena <- read_csv(here("data",id,session,"lena","output","bin","10mins_midnight_CRTRUE_RW5min_alldata","lena_10mins_midnight_CRTRUE_RW5min_alldata.csv")) %>% 
  mutate(id = id, session = session)
lena$clock_time_start <- with_tz(lena$dateTimeStart_UTC,  "America/Los_Angeles")
lena$clock_time_end <- with_tz(lena$dateTimeEnd_UTC,  "America/Los_Angeles")

lena <- lena %>% filter(clock_time_start > start_time, clock_time_end < stop_time)

#Add nap category to lena windows
lena$nap_time <- NA
for (i in 1:nrow(lena)) {
  temp_nap <- NULL
  temp_nap <- p %>% filter(time >= lena$clock_time_start[i], time < lena$clock_time_end[i]) 
  if (!is.null(temp_nap)) {
    lena$nap_time[i] <- temp_nap %>% summarize(prop_time = mean(nap_period, na.rm = T)) %>% pull(prop_time)
  }
}

#Add posture summary categories to lena windows
lena <- lena %>% mutate(
  sit_time = NA,
  held_time = NA,
  prone_time = NA,
  supine_time = NA,
  upright_time = NA,
)


for (i in 1:nrow(lena)) {
  temp_posture <- NULL
  temp_posture <- p %>% filter(time >= lena$clock_time_start[i], time < lena$clock_time_end[i], nap_period == 0) #
  if (!is.null(temp_posture)) {
    prop <- fct_count(temp_posture$pos, prop = T)
    lena$sit_time[i] <- prop %>% filter(f == "Sitting") %>% pull(p)
    lena$held_time[i] <- prop %>% filter(f == "Held") %>% pull(p)
    lena$prone_time[i] <- prop %>% filter(f == "Prone") %>% pull(p)
    lena$supine_time[i] <- prop %>% filter(f == "Supine") %>% pull(p)
    lena$upright_time[i] <- prop %>% filter(f == "Upright") %>% pull(p)
  }
}

write_csv(lena, here("data",id,session, "synced_data", glue("lena_imu_{who}.csv")))

# lena_long <- lena %>% pivot_longer(sit_time:upright_time, names_to = "posture", values_to = "prop") %>% 
#   mutate(posture = factor(posture, 
#                           levels = c("sit_time", "held_time", "prone_time", "supine_time", "upright_time"), 
#                           labels = c("Sitting", "Held", "Prone", "Supine", "Upright")))

