library(tidyverse)
library(here)
i_am(".here")
source(here("code","step3-lena","sync_lena_imu.R"))

synced_ppts <- read_csv(here("code","project_status","project_dashboard.csv")) %>% 
  filter(lena_processed == 2, infant_synced == 1)

walk2(synced_ppts$id, synced_ppts$session, ~ sync_lena_imu(id = .x, session = .y))

synced_ppts <- synced_ppts %>% filter(!(id == 110 & session == 4), !(id == 111 & session == 4), !(id == 116 & session == 1))
walk2(synced_ppts$id, synced_ppts$session, ~ sync_lena_imu(id = .x, session = .y, type = "split"))


