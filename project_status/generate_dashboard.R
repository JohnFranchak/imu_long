library(tidyverse)
library(here)
library(glue)
i_am(".here")

ids <- list.dirs(here("data"), recursive = F) %>% discard(str_detect(., pattern = "_template"))
sessions_dir <- map(ids, ~list.dirs(.x, recursive = F)) %>% flatten_chr
completed_paperwork <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/scanned_paperwork"))))
raw_videos <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/videos_raw"))))
converted_videos <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/videos_converted"))))
imu_files <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/imu"), recursive = T, ".csv")))
datavyu_files <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/coding"), ".opf")))
activity_exported <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/coding"), "activity.csv")))
biostamp_annotations <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/coding"), "biostamp_annotations")))
lena_downloaded <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/lena/its"), ".its")))
infant_synced <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/synced_data"), "mot_features_infant.RData")))
lena_synced <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/synced_data"), "lena_imu_infant.csv")))

dashboard <- tibble(sessions_dir, completed_paperwork, raw_videos, converted_videos, imu_files, datavyu_files, activity_exported, 
                    biostamp_annotations, infant_synced, lena_downloaded, lena_synced) %>% 
  separate(sessions_dir, into = c(NA, NA, NA, "id", "session"), sep = "/", remove = FALSE) %>%
  mutate(across(id:session, as.numeric)) %>% 
  arrange(id, session) %>% 
  filter(raw_videos > 0 | converted_videos > 0)

synced_ppts <- dashboard %>% filter(infant_synced == 1) %>% 
  transmute(sync_path = str_glue("{sessions_dir}/synced_data/mot_features_infant.RData")) %>% unlist

load(synced_ppts[1])
session_data <- tibble(id = session_param$id, session = session_param$session, start_time = session_param$start_time, end_time = session_param$end_time) %>% filter(FALSE)
for (ppt in synced_ppts) {
  rm(session_param)
  load(ppt)
  session_data <- bind_rows(session_data, tibble(id = session_param$id, session = session_param$session, start_time = session_param$start_time, end_time = session_param$end_time))
}

dashboard <- left_join(dashboard, session_data)
