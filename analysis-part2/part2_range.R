library(tidyverse)
library(av)
library(tidyvyur)
library(lubridate)
library(here)
source(here("code","analysis-part2","av_info.R"))
i_am(".here")


id <- 102
session <- 1
who <- "infant"

av_info <- av_info(id, session)

opf_pt1 <- list.files(here("data", id, session, "coding"), "*pt1.opf", full.names = T)
opf_pt2 <- list.files(here("data", id, session, "coding"), "*pt2.opf", full.names = T)

pt1 <- read_opf(opf_pt1)[["position"]]
pt2 <- read_opf(opf_pt2)[["position"]]

last_pt1_offset <- pt1$offset_s[nrow(pt1)]
pt1_video_length <- av_info %>% filter(str_detect(filename, "pt1")) %>% pull(duration)
buffer <- pt1_video_length - last_pt1_offset + 60

load(here("data",id, session, "synced_data", "mot_features_infant.RData"))

pt2_start_time <- session_param$end_time_coded + seconds(buffer)
pt2_end_time <- pt2_start_time + seconds(pt1$offset_s[nrow(pt2)])
