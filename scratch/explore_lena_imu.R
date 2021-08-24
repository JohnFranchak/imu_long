library(tidyverse)
library(here)
library(lubridate)

load(file = here("102","1","synced_data","synced_data.RData"))
start_time <- min(p$clock_time)
stop_time <- max(p$clock_time)

lena <- read_csv(here("102","1","synced_data", "102_1_10mins.csv")) 

ggplot(p) + 
  geom_path(aes(x = clock_time, y =.975,color = posture, group = 1L), size = 20) + 
  scale_x_datetime(date_break = "1 hour", date_labels = "%H:%M") + xlab("") + 
  ylim(.95,1.1) + theme(legend.position = "top") 

lena <- lena %>% mutate(scaled_ctc = convTurnCount/32/10 + 1,
                        scaled_tvn = TVN/147.09/10 + 1,
                        scaled_cry = childCryScnds/89.95/10 + 1,
                        clock_time = dateTimeStart_UTC + seconds(150) - hours(7),
                        clock_time_start = clock_time - seconds(150),
                        clock_time_end = clock_time + seconds(150)) %>% 
                filter(clock_time > start_time, clock_time < stop_time)


ggplot(p) + 
  geom_path(aes(x = clock_time, y =.975,color = posture, group = 1L), size = 20) + 
  geom_point(data = lena, aes(x = clock_time, y = scaled_ctc)) + 
  geom_point(data = lena, aes(x = clock_time, y = scaled_tvn), color = "red") +
  geom_point(data = lena, aes(x = clock_time, y = scaled_cry), color = "orange") +
  scale_x_datetime(date_break = "1 hour", date_labels = "%H:%M") + xlab("") + 
  ylim(.95,1.1) + theme(legend.position = "top") 

#Add posture summary categories to lena windows
lena <- lena %>% mutate(
  sit_time = NA,
  held_time = NA,
  prone_time = NA,
  supine_time = NA,
  upright_time = NA,
)

for (i in 1:nrow(tv)) {
  temp_posture <- p %>% filter(clock_time >= lena$clock_time_start[i], clock_time < lena$clock_time_end[i], nap == 1)
  if (!is.null(temp_posture)) {
    prop <- fct_count(temp_posture$posture, prop = T)
    lena$sit_time[i] <- prop %>% filter(f == "Sitting") %>% pull(p)
    lena$held_time[i] <- prop %>% filter(f == "Held") %>% pull(p)
    lena$prone_time[i] <- prop %>% filter(f == "Prone") %>% pull(p)
    lena$supine_time[i] <- prop %>% filter(f == "Supine") %>% pull(p)
    lena$upright_time[i] <- prop %>% filter(f == "Upright") %>% pull(p)
  }
}

ggplot(lena, aes(x = sit_time, y = adultWordCnt)) + geom_point()

lena_long <- lena %>% pivot_longer(sit_time:upright_time, names_to = "posture", values_to = "prop") %>% 
  mutate(posture = factor(posture, 
                          levels = c("sit_time", "held_time", "prone_time", "supine_time", "upright_time"), 
                          labels = c("Sitting", "Held", "Prone", "Supine", "Upright")))

ggplot(lena_long, aes(y = prop, x = posture)) + geom_boxplot()
