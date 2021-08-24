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
                        clock_time = dateTimeStart_UTC + seconds(150) - hours(7)) %>% 
                filter(clock_time > start_time, clock_time < stop_time)


ggplot(p) + 
  geom_path(aes(x = clock_time, y =.975,color = posture, group = 1L), size = 20) + 
  geom_point(data = lena, aes(x = clock_time, y = scaled_ctc)) + 
  geom_point(data = lena, aes(x = clock_time, y = scaled_tvn), color = "red") + 
  scale_x_datetime(date_break = "1 hour", date_labels = "%H:%M") + xlab("") + 
  ylim(.95,1.1) + theme(legend.position = "top") 
