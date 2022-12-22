av_info <- function(id, session, who = "infant") {

require(tidyverse)
require(av)
require(here)
i_am(".here")
# 
# id <- 102
# session <- 1
# who <- "infant"

load(here("data",id,session, "synced_data", str_glue("mot_features_{who}.RData")))

files_to_cat <- list.files(here("data", id, session, "videos_converted"), ".mov|.mp4", full.names = T)
files_short <- list.files(here("data", id, session, "videos_converted"), ".mov|.mp4")

av_info <- map_dfr(files_to_cat, ~av_media_info(.x)[["video"]]) %>% 
  bind_cols(tibble(duration = map_dbl(files_to_cat, ~av_media_info(.x)[["duration"]]))) %>% 
  bind_cols(tibble(filename = files_short)) 
write_csv(av_info, here("data", id, session, "videos_converted", str_glue("av_info.csv")) )
return(av_info)
}