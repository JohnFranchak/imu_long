# devtools::install_github("JohnFranchak/tidyvyur")

library(tidyverse)
library(tidyvyur)
library(here)
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
lena_downloaded <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/lena/its"), "lena.its")))
lena_processed <-  map_int(sessions_dir, ~ length(list.dirs(str_glue("{.x}/lena/output/bin"))))
infant_synced <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/synced_data"), "mot_features_infant.RData")))
lena_synced <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/synced_data"), "lena_imu_infant.csv")))

#check coding
opf_files <- map(sessions_dir, ~ list.files(str_glue("{.x}/coding"), "*pt1.opf", full.names = T)) %>% set_names(sessions_dir)

position_primary <- lena_downloaded * 0 
position_rel <- position_primary
for (i in 1:length(opf_files)) {
  if (str_detect(opf_files[i], ".opf")) {
    tmp_opf <- read_opf(opf_files[[i]])
    position_primary[i] <- ifelse(is.null(nrow(tmp_opf[['position']])), 0, nrow(tmp_opf[['position']]))
    position_rel[i] <- ifelse(is.null(nrow(tmp_opf[['position_rel']])), 0, nrow(tmp_opf[['position_rel']]))
  }
}


dashboard <- tibble(sessions_dir, completed_paperwork, raw_videos, converted_videos, imu_files, datavyu_files, position_primary, position_rel, activity_exported, 
                    biostamp_annotations, infant_synced, lena_downloaded, lena_processed, lena_synced) %>% 
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
  print(ppt)
  load(ppt)
  session_data <- bind_rows(session_data, tibble(id = session_param$id, session = session_param$session, start_time = session_param$start_time, end_time = session_param$end_time))
}

dashboard <- left_join(dashboard, session_data)

synced_ppts <- dashboard %>% filter(infant_synced == 1) %>% 
  transmute(sync_path = str_glue("{sessions_dir}/synced_data/model_performance_infant.RData")) %>% unlist

session_data <- tibble(sessions_dir = "NA", overall_accuracy = 1, balanced_accuracy = 1, held_accuracy = 1,
                       prone_accuracy = 1, sitting_accuracy = 1, supine_accuracy = 1, upright_accuracy = 1) %>% filter(FALSE)
for (ppt in synced_ppts) {
  rm(res)
  print(ppt)
  load(ppt)
  temp_acc <- res$`Class Level Results`[[1]]$`Balanced Accuracy` %>% set_names(res$`Class Level Results`[[1]]$`Class`)
  session_data <- bind_rows(session_data, tibble(sessions_dir = str_remove(ppt,"/synced_data/model_performance_infant.RData"), overall_accuracy = res$`Overall Accuracy`, balanced_accuracy = res$`Balanced Accuracy`, 
                                                 held_accuracy = temp_acc[["Held"]], prone_accuracy = temp_acc[["Prone"]], sitting_accuracy = temp_acc[["Sitting"]], 
                                                 supine_accuracy = temp_acc[["Supine"]], upright_accuracy = temp_acc[["Upright"]]))
}

dashboard_full <- left_join(dashboard, session_data, by = "sessions_dir")

dashboard_full %>% write_csv(here("code","project_status", "project_dashboard.csv"))

system("quarto render")
