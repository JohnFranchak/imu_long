library(tidyverse)
library(here)
i_am(".here")
source(here("code","analysis-part2","part2_range.R"))

synced_ppts <- read_csv(here("code","project_status","project_dashboard.csv")) %>% 
  filter(pt2_processed == 1)

synced_ppts <- synced_ppts %>% filter(!(id == 110 & session == 4))
walk2(synced_ppts$id, synced_ppts$session, ~ part2_range(id = .x, session = .y, who = "infant", type = "position"))

synced_ppts <- synced_ppts %>% filter(!(id == 110 & session == 4), !(id == 111 & session == 4))
walk2(synced_ppts$id, synced_ppts$session, ~ part2_range(id = .x, session = .y, who = "infant", type = "sitting"))
