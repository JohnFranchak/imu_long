# devtools::install_github("JohnFranchak/tidyvyur")

library(tidyverse)
library(tidyvyur)
library(lubridate)
library(here)
i_am(".here")

ids <- list.dirs(here("data"), recursive = F) %>% purrr::discard(str_detect(., pattern = "_template")) %>% purrr::discard(str_detect(., pattern = "_RAs"))
sessions_dir <- map(ids, ~list.dirs(.x, recursive = F)) %>% flatten_chr
completed_paperwork <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/scanned_paperwork"))))
raw_videos <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/videos_raw"))))
converted_videos <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/videos_converted"),"mp4|mov")))
imu_files <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/imu"), recursive = T, ".csv")))
datavyu_files <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/coding"), ".opf")))
activity_exported <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/coding"), "activity.csv")))
biostamp_annotations <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/coding"), "biostamp_annotations")))
lena_downloaded <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/lena/its"), "lena.its")))
lena_processed <-  map_int(sessions_dir, ~ length(list.dirs(str_glue("{.x}/lena/output/bin"))))
infant_synced <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/synced_data"), "mot_features_infant.RData")))
lena_synced <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/synced_data"), "lena_imu_infant.csv")))
pt2_processed <- map_int(sessions_dir, ~ length(list.files(str_glue("{.x}/synced_data"), "position_agreement_infant_pt2.csv")))

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
                    biostamp_annotations, infant_synced, lena_downloaded, lena_processed, lena_synced, pt2_processed) %>% 
  separate(sessions_dir, into = c(NA, NA, NA, "id", "session"), sep = "/", remove = FALSE) %>%
  mutate(across(id:session, as.numeric)) %>% 
  arrange(id, session) %>% 
  filter(raw_videos > 0 | converted_videos > 0)

synced_ppts <- dashboard %>% filter(infant_synced == 1) %>% 
  transmute(sync_path = str_glue("{sessions_dir}/synced_data/mot_features_infant.RData")) %>% unlist

# Get dates and times from ppt_info
ppt_info <- read_csv(here("data", "ppt_info.csv"))
ppt_info  <- ppt_info %>%  mutate(agemo = as.numeric((mdy(date)-mdy(infant_dob))/(365.25/12))) 

session_data <- ppt_info %>% select(id, session, agemo, date, start_time, end_time) %>% drop_na
dashboard <- left_join(dashboard, session_data)

dashboard %>% write_csv(here("code","project_status", "project_dashboard.csv"))

# Get model performance
# last_updated <- file.info(here("code","project_status", "project_dashboard.csv"))$mtime
# old_dashboard <- read_csv(here("code","project_status", "project_dashboard.csv")) %>% select(sessions_dir, overall_accuracy:upright_accuracy)
# 
# synced_ppts <- dashboard %>% filter(infant_synced == 1) %>% 
#   transmute(sync_path = str_glue("{sessions_dir}/synced_data/model_performance_infant.RData")) %>% unlist %>% 
#   keep(~ file.info(.x)$mtime > last_updated)
# 
# session_data <- old_dashboard %>% filter(FALSE)

# for (ppt in synced_ppts) {
#   rm(res)
#   print(ppt)
#   load(ppt)
#   temp_acc <- res$`Class Level Results`[[1]]$`Balanced Accuracy` %>% set_names(res$`Class Level Results`[[1]]$`Class`)
#   session_data <- bind_rows(session_data, tibble(sessions_dir = str_remove(ppt,"/synced_data/model_performance_infant.RData"), overall_accuracy = res$`Overall Accuracy`, balanced_accuracy = res$`Balanced Accuracy`, 
#                                                  held_accuracy = temp_acc[["Held"]], prone_accuracy = temp_acc[["Prone"]], sitting_accuracy = temp_acc[["Sitting"]], 
#                                                  supine_accuracy = temp_acc[["Supine"]], upright_accuracy = temp_acc[["Upright"]]))
# }
# old_dashboard <- old_dashboard %>% filter(!(sessions_dir %in% str_remove(synced_ppts,"/synced_data/model_performance_infant.RData")))
# old_dashboard <- old_dashboard %>% bind_rows(session_data)

# dashboard_full <- left_join(dashboard, old_dashboard, by = "sessions_dir")

# dashboard_full %>% write_csv(here("code","project_status", "project_dashboard.csv"))

