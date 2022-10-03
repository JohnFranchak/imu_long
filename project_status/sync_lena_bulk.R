library(tidyverse)
library(here)
i_am(".here")
source(here("code","step3-lena","sync_lena_imu.R"))

synced_ppts <- read_csv(here("code","project_status","project_dashboard.csv")) %>% 
  filter(lena_processed == 2, infant_synced == 1, lena_synced == 0)

walk2(synced_ppts$id, synced_ppts$session, ~ sync_lena_imu(id = .x, session = .y))

source(here("code","project_status","generate_dashboard.R"))
