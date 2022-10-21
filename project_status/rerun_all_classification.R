library(tidyverse)
library(here)
i_am(".here")
source(here("code","step2-classification","run_classification_split.R"))

synced_ppts <- read_csv(here("code","project_status","project_dashboard.csv")) %>% 
  filter(infant_synced == 1)

walk2(synced_ppts$id, synced_ppts$session, ~ run_classification_split(id = .x, session = .y, who = "infant", type = "group"))

#Filter out ppts missing categories who can't do split fits
synced_ppts <- synced_ppts %>% filter(!(id == 110 & session == 4), !(id == 116 & session == 1))
walk2(synced_ppts$id, synced_ppts$session, ~ run_classification_split(id = .x, session = .y, who = "infant", type = "split"))
