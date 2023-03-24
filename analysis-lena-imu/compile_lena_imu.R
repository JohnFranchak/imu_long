rm(list = ls())
library(tidyverse)
library(here)
library(lubridate)
library(patchwork)
i_am(".here")

incl_ids <- c(801, 901, 1001, 1101, 10201, 10203, 10204, 10301, 10501, 10601, 10701, 10702, 10703, 10704,
              10802, 10901, 11001, 11002, 11004, 11102, 11103, 11104, 11202, 11501, 11601, 11901, 12001,
              12002, 12101, 12302, 12303, 12501, 12502, 13001)

# GROUP MODELS

synced_ppts <- read_csv(here("code","project_status","project_dashboard.csv")) %>% 
  filter(lena_synced == 1) %>% select(id, session, agemo)

ds <- pmap_dfr(synced_ppts, ~ read_csv(here("data",..1, ..2,"synced_data","lena_imu_infant.csv")) %>% add_column(age = ..3)) %>% 
  mutate(id_uni = id*100+session)

write_csv(ds, here("code","analysis-lena-imu","lena-imu-compiled.csv"))

# For Methods paper removing LENA data
ds %>% filter(id_uni %in% incl_ids) %>%
  select(clockTimeStart:clockTimeEnd, id:age) %>% 
  rename_with(janitor::make_clean_names) %>% 
  write_csv(here("code","analysis-lena-imu","imu-compiled.csv"))

# SPLIT MODELS
synced_ppts <- synced_ppts %>% filter(!(id == 110 & session == 4), !(id == 111 & session == 4), !(id == 116 & session == 1))
ds <- pmap_dfr(synced_ppts, ~ read_csv(here("data",..1, ..2,"synced_data","lena_imu_infant_split.csv")) %>% add_column(age = ..3))  %>% 
  mutate(id_uni = id*100+session)

write_csv(ds, here("code","analysis-lena-imu","lena-imu-compiled-split.csv"))

# For Methods paper removing LENA data
ds %>%  filter(id_uni %in% incl_ids) %>%
  select(clockTimeStart:clockTimeEnd, id:age) %>% 
  rename_with(janitor::make_clean_names) %>% 
  write_csv(here("code","analysis-lena-imu","imu-compiled-split.csv"))
