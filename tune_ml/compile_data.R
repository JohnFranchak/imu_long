library(here)
library(janitor)
library(tidyverse)
#LOAD DATA
rds <- list.files(here("tune_ml","mot_features"), pattern = ".RData", full.names = T)

load(rds[1])
slide_all <- slide %>% 
  mutate(id = session_param$id*100 + session_param$session) %>% 
  filter(F)
session <- as_tibble(session_param) %>% filter(F)

for (r in rds) {
  load(r)

  slide_all <- slide %>% 
    mutate(id = session_param$id*100 + session_param$session) %>% 
    bind_rows(slide_all)
  session <- session %>% bind_rows(as_tibble(session_param))
  
  rm(slide)
  rm(session_param)
}

slide_filt <- slide_all %>% filter(video_period == 1) %>% select(-video_period, -nap_period)

#CODE FACTORS
slide_filt$code = ifelse(slide_filt$code == "sr", "ss", slide_filt$code)
slide_filt$code <- factor(slide_filt$code, levels = c("hs", "l","p","ss","u"), labels = c("Held", "Supine","Prone","Sitting","Upright"))
slide_filt <- slide_filt %>% filter(code_prop > .75) %>% drop_na(code) %>% select(-code_prop, -(time:time_to))
not_all_na <- function(x) !any(is.na(x))
slide_filt <- slide_filt %>% select_if(not_all_na)

save(slide_all, slide_filt, session, file = "tune_ml/compiled_data.RData")

