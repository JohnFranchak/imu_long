rm(list = ls())
library(tidyverse)
library(here)

synced_ppts <- read_csv(here("code","project_status","project_dashboard.csv")) %>% 
  filter(lena_synced == 1) %>% select(id, session, agemo)
