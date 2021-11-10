rm(list = ls())
library(tidyverse)
library(here)
library(lubridate)
library(patchwork)
library(janitor)
library(rstatix)

#SERVER
# i_am(".here")
# ds <- read_csv(here("code","analysis-lena-imu","lena-imu-compiled.csv"))

#MAC
ds <- read_csv(here("analysis-lena-imu","lena-imu-compiled.csv"))
ds$id <- factor(ds$id*100+ds$session)

ds <- ds %>% select(-(subjID:recIdEnd), -(roll.key:Bin.Mins))
ds <- ds %>% rename_with(janitor::make_clean_names)

lena_pos_corrs <- . %>% cor_mat(adult_word_cnt, conv_turn_count, child_utt_cnt, child_cry_scnds, tvn, sit_time:upright_time) %>% cor_mark_significant()

ds %>% lena_pos_corrs()
ds %>% group_by(id) %>% group_map(~ .x %>% lena_pos_corrs())

ggplot(ds, aes(x = sit_time, y = adult_word_cnt, color = id)) + 
  geom_point() + 
  facet_wrap("id")

ggplot(ds, aes(x = upright_time, y = child_utt_cnt, color = id)) + 
  geom_point() + 
  facet_wrap("id")
