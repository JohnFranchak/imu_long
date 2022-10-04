rm(list = ls())
library(tidyverse)
library(here)
library(lubridate)
library(patchwork)
library(janitor)
library(rstatix)
library(scales)

#SERVER
i_am(".here")
ds <- read_csv(here("code","analysis-lena-imu","lena-imu-compiled.csv"))

#MAC
ds <- read_csv(here("analysis-lena-imu","lena-imu-compiled.csv"))


ds$id_uni <- factor(ds$id*100+ds$session)
ds <- ds %>% filter(id_uni != 9909)

ds <- ds %>% select(-(subjID:recIdEnd), -(roll.key:Bin.Mins), clockTimeStart)
ds <- ds %>% rename_with(janitor::make_clean_names)

lena_pos_corrs <- . %>% cor_mat(adult_word_cnt, conv_turn_count, child_utt_cnt, child_cry_scnds, tvn, sit_time:upright_time) %>% cor_mark_significant()

ds %>% lena_pos_corrs()
ds %>% group_by(id_uni) %>% group_map(~ .x %>% lena_pos_corrs())

ggplot(ds, aes(x = sit_time, y = adult_word_cnt)) + 
  geom_point() + 
  facet_wrap("id_uni")

ds_long <- ds %>% pivot_longer(cols = sit_time:upright_time, names_to = "position", values_to = "prop") 

ggplot(ds_long, aes(x = clock_time_start_2, y = prop, fill = position)) + 
  geom_bar(stat = "identity") + 
  facet_wrap("id_uni") + 
  scale_x_time(breaks = breaks_width("2 hour"))
