library(tidyverse)
library(here)
library(lubridate)
i_am(".here")

id <- 99
session <- 8

p <- read_csv(here("data",id,session,"synced_data","position_predictions_infant.csv")) %>% 
  mutate(pos = factor(pos, levels = c("Prone","Upright","Held","Sitting","Supine")))
p$time <- with_tz(p$time, "America/Los_Angeles")
start_time <- min(p$time)
stop_time <- max(p$time)

lena <- read_csv(here("data",id,session,"lena","output","bin","10mins_midnight_CRTRUE_RW5min_alldata","lena_10mins_midnight_CRTRUE_RW5min_alldata.csv")) 
lena$clock_time_start <- with_tz(lena$dateTimeStart_UTC,  "America/Los_Angeles")
lena$clock_time_end <- with_tz(lena$dateTimeEnd_UTC,  "America/Los_Angeles")

ggplot(p) + 
  geom_path(aes(x = time, y =.975,color = pos, group = 1L), size = 20) + 
  scale_x_datetime(date_break = "1 hour", date_labels = "%H:%M") + xlab("") + 
  ylim(.95,1.1) + theme(legend.position = "top") 

lena <- lena %>% filter(clock_time_start > start_time, clock_time_end < stop_time)

ggplot(p) + 
  #geom_path(aes(x = time, y =.975,color = pos, group = 1L), size = 20) + 
  geom_point(data = lena, aes(x = clock_time_start, y = convTurnCount)) +
  geom_line(data = lena, aes(x = clock_time_start, y = convTurnCount)) +
  #geom_point(data = lena, aes(x = clock_time, y = scaled_tvn), color = "red") +
  #geom_point(data = lena, aes(x = clock_time, y = scaled_cry), color = "orange") +
  scale_x_datetime(date_break = "1 hour", date_labels = "%H:%M") + xlab("")

#Add posture summary categories to lena windows
lena <- lena %>% mutate(
  sit_time = NA,
  held_time = NA,
  prone_time = NA,
  supine_time = NA,
  upright_time = NA,
)

for (i in 1:nrow(lena)) {
  temp_posture <- p %>% filter(time >= lena$clock_time_start[i], time < lena$clock_time_end[i], nap_period == 0)
  if (!is.null(temp_posture)) {
    prop <- fct_count(temp_posture$pos, prop = T)
    lena$sit_time[i] <- prop %>% filter(f == "Sitting") %>% pull(p)
    lena$held_time[i] <- prop %>% filter(f == "Held") %>% pull(p)
    lena$prone_time[i] <- prop %>% filter(f == "Prone") %>% pull(p)
    lena$supine_time[i] <- prop %>% filter(f == "Supine") %>% pull(p)
    lena$upright_time[i] <- prop %>% filter(f == "Upright") %>% pull(p)
  }
}

ggplot(lena, aes(x = upright_time, y = adultWordCnt)) + geom_point()
rstatix::cor_test(lena, upright_time, adultWordCnt)

lena_long <- lena %>% pivot_longer(sit_time:upright_time, names_to = "posture", values_to = "prop") %>% 
  mutate(posture = factor(posture, 
                          levels = c("sit_time", "held_time", "prone_time", "supine_time", "upright_time"), 
                          labels = c("Sitting", "Held", "Prone", "Supine", "Upright")))

ggplot(lena_long, aes(y = prop, x = posture)) + geom_boxplot()

ggplot(lena_long, aes(y = prop, x = clock_time_start, fill = posture)) + geom_bar(position = "stack", stat = "identity")

