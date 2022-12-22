library(tidyverse)
library(av)
library(tidyvyur)
library(lubridate)
library(here)
source(here("code","analysis-part2","av_info.R"))
i_am(".here")

id <- 102
session <- 3
who <- "infant"

av_info <- av_info(id, session)

opf_pt1 <- list.files(here("data", id, session, "coding"), "*pt1.opf", full.names = T)
opf_pt2 <- list.files(here("data", id, session, "coding"), "*pt2.opf", full.names = T)

pt1 <- read_opf(opf_pt1)[["position"]]
pt2 <- read_opf(opf_pt2)[["position"]]

last_pt1_offset <- pt1$offset_s[nrow(pt1)]
pt1_video_length <- av_info %>% filter(str_detect(filename, "pt1")) %>% pull(duration)
buffer <- pt1_video_length - last_pt1_offset + 40 #40 sec average (range 36-40 of between video gaps)

load(here("data",id, session, "synced_data", "mot_features_infant.RData"))

pt2_start_time <- session_param$end_time_coded + seconds(buffer)
pt2_end_time <- pt2_start_time + seconds(pt2$offset_s[nrow(pt2)])

ds <- read_csv(here("data",id, session, "synced_data", "position_predictions_infant_group.csv"))
ds$time <- with_tz(ds$time, "America/Los_Angeles")
ds_coded <-ds %>% filter_by_time(time, .start_date = pt2_start_time, .end_date = pt2_end_time)
ds_coded$code <- as.character(NA)

activity <- pt2
activity$onset <- pt2_start_time + seconds(activity$onset_s)
activity$offset <- pt2_start_time + seconds(activity$offset_s)
activity$pos = ifelse(activity$pos == "sr", "ss", activity$pos)
activity$pos <- factor(activity$pos, levels = c("hs", "l","p","ss","u"), labels = c("Held", "Supine","Prone","Sitting","Upright"))


for (i in 1:nrow(activity)) {
  ds_coded[between_time(ds_coded$time, activity$onset[i], activity$offset[i]),]$code <- activity$pos[i]
} 
print(fct_count(ds_coded$pos, prop = T))
print(fct_count(ds_coded$code, prop = T))

write_csv(ds_coded, here("data",id, session, "synced_data", "position_agreement_infant_pt2.csv"))

ggplot(ds_coded) + 
  geom_bar(aes(x = time, y = 1, fill = pos), stat = "identity") + 
  geom_bar(aes(x = time, y = -1, fill = code), stat = "identity") + 
  geom_hline(yintercept = 0, size = 5, color = "white") + theme_minimal() + 
  scale_y_continuous(breaks = c(-.5, .5), labels = c("Human", "Model"))

ggsave(here("data",id, session, "synced_data", "pt2-agreement.jpg"), width = 12, height = 4)

