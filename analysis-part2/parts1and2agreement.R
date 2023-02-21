library(tidyverse)
library(av)
library(tidyvyur)
library(lubridate)
library(here)
library(timetk)
library(scales)
library(hms)
i_am(".here")
source(here("code","analysis-part2","av_info.R"))

theme_update(text = element_text(size = 14),
             axis.text.x = element_text(size = 14, color = "black"), axis.title.x = element_text(size = 16),
             axis.text.y = element_text(size = 14,  color = "black"), axis.title.y = element_text(size = 16), 
             panel.background = element_blank(),panel.border = element_blank(), panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(), axis.line = element_blank(), 
             legend.key = element_rect(fill = "white")) 



id <- 120
session <- 1
who <- "infant"
type <- "position" # sitting/position

av_info <- av_info(id, session)

opf_pt1 <- list.files(here("data", id, session, "coding"), "*pt1.opf", full.names = T)
opf_pt2 <- list.files(here("data", id, session, "coding"), "*pt2.opf", full.names = T)

pt1 <- read_opf(opf_pt1)[["position"]] %>% filter(pos != ".")
pt2 <- read_opf(opf_pt2)[["position"]]

last_pt1_offset <- pt1$offset_s[nrow(pt1)]
pt1_video_length <- av_info %>% filter(str_detect(filename, "pt1")) %>% pull(duration)
buffer <- pt1_video_length - last_pt1_offset + 40 #40 sec average (range 36-40 of between video gaps)

load(here("data",id, session, "synced_data", "mot_features_infant.RData"))

pt2_start_time <- session_param$end_time_coded + seconds(buffer)
pt2_end_time <- pt2_start_time + seconds(pt2$offset_s[nrow(pt2)])

if(type == "position") {
  ds <- read_csv(here("data",id, session, "synced_data", str_glue("position_predictions_infant_group.csv")))
} else if (type == "sitting") {
  ds <- read_csv(here("data",id, session, "synced_data", str_glue("position_predictions_infant_sitting.csv")))
} else if (type == "split") {
  ds <- read_csv(here("data",id, session, "synced_data", str_glue("position_predictions_infant_split.csv")))
}
ds$time <- with_tz(ds$time, "America/Los_Angeles")
ds_coded <-ds %>% filter_by_time(time, .start_date = session_param$start_time, .end_date =session_param$end_time)
ds_coded$code <- as.character(NA)

activity <- pt1
activity$onset <- session_param$start_time_coded + seconds(activity$onset_s) -  seconds(activity$onset_s[1])
activity$offset <- session_param$start_time_coded + seconds(activity$offset_s)
activity$pos = ifelse(activity$pos == "sr", "ss", activity$pos)
activity$pos <- factor(activity$pos, levels = c("n","hs", "l","p","ss","u"), labels = c("Nap","Held", "Supine","Prone","Sitting","Upright"))

for (i in 1:nrow(activity)) {
  ds_coded[between_time(ds_coded$time, activity$onset[i], activity$offset[i]),]$code <- activity$pos[i]
} 

activity <- pt2
activity$onset <- pt2_start_time + seconds(activity$onset_s)
activity$offset <- pt2_start_time + seconds(activity$offset_s)
activity$pos = ifelse(activity$pos == "sr", "ss", activity$pos)
activity$pos <- factor(activity$pos, levels = c("n","hs", "l","p","ss","u"), labels = c("Nap","Held", "Supine","Prone","Sitting","Upright"))

for (i in 1:nrow(activity)) {
  ds_coded[between_time(ds_coded$time, activity$onset[i], activity$offset[i]),]$code <- activity$pos[i]
} 


pos_levels <- c("Nap","Supine", "Prone", "Sitting", "Upright", "Held")
pal <-  c("gray","#E69F00","#56B4E9", "#009E73","#F0E442", "#0072B2") %>% 
  set_names(pos_levels)

ds_timeline <- ds_coded %>% 
  filter(exclude_period == 0) %>% 
  mutate(time = as_hms(time),
        pos = ifelse(nap_period == 1, "Nap", pos),
         code = ifelse(nap_period == 1, "Nap", code),
        pos = factor(pos, levels = pos_levels),
        code = factor(code, levels = pos_levels))

lims <- as_hms(c('10:00:00', '20:00:00'))
hour_breaks = as_hms(c('10:00:00', '11:00:00', '12:00:00', '13:00:00', '14:00:00', '15:00:00', '16:00:00', '17:00:00', '18:00:00', '19:00:00', '20:00:00'))
label_breaks = c("10am","11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm")

ggplot() + 
  geom_tile(data = ds_timeline, mapping = aes(x = time, y = -.5, fill = pos), stat = "identity") + 
  geom_tile(data = drop_na(ds_timeline, code), mapping = aes(x = time, y = .5, fill = code), stat = "identity") + 
  scale_x_time(breaks = breaks_width("1 hour"), name = "") + 
  scale_fill_manual(values = pal, name = "") + 
  scale_y_continuous(breaks = c(-.5, .5), labels = c("Model", "Human"), name = "") + 
  theme(legend.position = "bottom")
ggsave(here("data",id, session, "synced_data", str_glue("fullday-agreement-{type}.pdf")), width = 12, height = 4)

