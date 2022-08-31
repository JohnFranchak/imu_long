library(tidyverse)
library(here)
i_am(".here")
source(here("code","step3-lena","process_its.R"))

synced_ppts <- read_csv(here("code","project_status","project_dashboard.csv")) %>% 
  filter(lena_processed == 0, lena_downloaded == 1)

walk2(synced_ppts$id, synced_ppts$session, ~ process_its(id = .x, session = .y))
walk(synced_ppts$sessions_dir, ~ file.remove(str_glue("{.x}/synced_data/lena_infant_imu.csv")))

source("generate_dashboard.R")
