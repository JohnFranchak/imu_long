library(here)
library(janitor)
library(tidyverse)
i_am(".here")
#LOAD DATA

ids <- list.dirs(here("data"), recursive = F) %>% discard(str_detect(., pattern = "_template"))
sessions_dir <- map(ids, ~list.dirs(.x, recursive = F)) %>% flatten_chr
infant_synced <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/synced_data"), "mot_features_infant.RData")))
synced_session_dirs <- sessions_dir[infant_synced ==1]

rds <- map_chr(synced_session_dirs, ~ str_glue("{.x}/synced_data/mot_features_infant.RData"))

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

save(slide_filt, session, file = "tune_ml/compiled_data_lite.RData")

