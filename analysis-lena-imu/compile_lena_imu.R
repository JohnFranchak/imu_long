rm(list = ls())
library(tidyverse)
library(here)
library(lubridate)
library(patchwork)
i_am(".here")

synced_ppts <- read_csv(here("code","project_status","project_dashboard.csv")) %>% 
  filter(lena_synced == 1) %>% select(id, session, agemo)

ds <- pmap_dfr(synced_ppts, ~ read_csv(here("data",..1, ..2,"synced_data","lena_imu_infant.csv")) %>% add_column(age = ..3))

write_csv(ds, here("code","analysis-lena-imu","lena-imu-compiled.csv"))

ds %>% select(clockTimeStart:clockTimeEnd, id:age) %>% 
  rename_with(janitor::make_clean_names) %>% 
  write_csv(here("code","analysis-lena-imu","imu-compiled.csv"))

