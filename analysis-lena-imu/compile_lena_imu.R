rm(list = ls())
library(tidyverse)
library(here)
library(lubridate)
library(patchwork)
i_am(".here")

# GROUP MODELS

synced_ppts <- read_csv(here("code","project_status","project_dashboard.csv")) %>% 
  filter(lena_synced == 1) %>% select(id, session, agemo)

ds <- pmap_dfr(synced_ppts, ~ read_csv(here("data",..1, ..2,"synced_data","lena_imu_infant.csv")) %>% add_column(age = ..3))

write_csv(ds, here("code","analysis-lena-imu","lena-imu-compiled.csv"))

# For BRM paper removing LENA data
ds %>% select(clockTimeStart:clockTimeEnd, id:age) %>% 
  rename_with(janitor::make_clean_names) %>% 
  write_csv(here("code","analysis-lena-imu","imu-compiled.csv"))

# SPLIT MODELS
synced_ppts <- synced_ppts %>% filter(!(id == 110 & session == 4), !(id == 111 & session == 4), !(id == 116 & session == 1))
ds <- pmap_dfr(synced_ppts, ~ read_csv(here("data",..1, ..2,"synced_data","lena_imu_infant_split.csv")) %>% add_column(age = ..3))

write_csv(ds, here("code","analysis-lena-imu","lena-imu-compiled-split.csv"))

# For BRM paper removing LENA data
ds %>% select(clockTimeStart:clockTimeEnd, id:age) %>% 
  rename_with(janitor::make_clean_names) %>% 
  write_csv(here("code","analysis-lena-imu","imu-compiled-split.csv"))
