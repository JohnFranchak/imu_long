rm(list = ls())
library(tidyverse)
library(here)
library(lubridate)
library(patchwork)
library(janitor)
library(rstatix)
library(scales)
library(lme4)
library(lmerTest)
theme_update(text = element_text(size = 14),
             axis.text.x = element_text(size = 14, color = "black"), axis.title.x = element_text(size = 16),
             axis.text.y = element_text(size = 14,  color = "black"), axis.title.y = element_text(size = 16), 
             panel.background = element_blank(),panel.border = element_blank(), panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(), axis.line = element_blank(), 
             legend.key = element_rect(fill = "white")) 

#SERVER
i_am(".here")
ds <- read_csv(here("code","analysis-lena-imu","lena-imu-compiled.csv"))
ds <- read_csv(here("code","analysis-lena-imu","lena-imu-compiled-split.csv"))

#MAC
ds <- read_csv(here("analysis-lena-imu","lena-imu-compiled.csv"))


ds$id_uni <- factor(ds$id*100+ds$session)

ds <- ds %>% select(-(subjID:recIdEnd), -(roll.key:Bin.Mins), clockTimeStart)
ds <- ds %>% rename_with(janitor::make_clean_names)

lena_pos_corrs <- . %>% cor_mat(adult_word_cnt, conv_turn_count, child_utt_cnt, child_cry_scnds, tvn, sit_time:upright_time) %>% cor_mark_significant()

ds %>% lena_pos_corrs()
ds %>% filter(age > 8) %>% group_by(id_uni) %>% group_map(~ .x %>% lena_pos_corrs())

ds %>% filter(age > 8) %>% lena_pos_corrs()

ds_long <- ds %>% pivot_longer(cols = sit_time:upright_time, names_to = "position", values_to = "prop") %>% 
  mutate(Position = factor(position,
                           levels = c("upright_time", "sit_time", "prone_time", "supine_time", "held_time"),
                           labels = c("Upright", "Sitting", "Prone", "Supine", "Held"))) %>% 
  arrange(Position, age)

pal <-  c("#F0E442","#009E73","#56B4E9", "#E69F00",  "#0072B2") %>%  set_names(unique(ds_long$Position))

ds_long %>% 
  ggplot(aes(x = prop, y = adult_word_cnt)) + facet_wrap("position") + 
  geom_point(alpha = .25) + geom_smooth(method = "lm", color = "red") + 
  xlab("Prop. Time in Position") + ylab("Number of Adult Words") + ylim(0, 1000)

ds_long %>% filter(position == "held_time") %>% 
  ggplot(aes(x = prop, y = adult_word_cnt)) + 
  geom_point(alpha = .25) + geom_smooth(method = "lm", color = "red") + 
  xlab("Prop. Time Held") + ylab("Number of Adult Words") + ylim(0, 1000)

positions <- unique(ds_long$position)
map(positions, ~ lmer(adult_word_cnt ~ prop+age + (1 + prop+age|id_uni), 
                      data = filter(ds_long, position == .x))) %>% 
  set_names(positions) %>% map(summary)

map(positions, ~ lmer(adult_word_cnt ~ prop+age + (1 + prop|id_uni), 
                      data = filter(ds_long, position == .x))) %>% 
  set_names(positions) %>% map(summary)

ds_long %>% filter(age < 8) %>% 
ggplot(aes(x = clock_time_start_2, y = prop, fill = Position)) + 
  geom_bar(stat = "identity") + 
  facet_wrap("id_uni", ncol = 1, strip.position = "left") + 
  scale_fill_manual(values = pal) + 
  scale_x_time(breaks = breaks_width("2 hour"), name = "") + 
  theme(legend.position = "bottom", 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) + 
  ylab("Individual Infants") + 
  ggtitle("4-7 months")
ggsave(here("analysis-lena-imu", "figures", "posture-timeline-younger.png"))


ds_long %>% filter(age >= 8) %>% 
  ggplot(aes(x = clock_time_start_2, y = prop, fill = Position)) + 
  geom_bar(stat = "identity") + 
  facet_wrap("id_uni", ncol = 1, strip.position = "left") + 
  scale_fill_manual(values = pal) + 
  scale_x_time(breaks = breaks_width("2 hour"), name = "") + 
  theme(legend.position = "bottom", 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) + 
  ylab("Individual Infants") + 
  ggtitle("11-14 months")
ggsave(here("analysis-lena-imu", "figures", "posture-timeline-older.png"))

ds_long %>% filter(age <= 8) %>% group_by(position) %>% get_summary_stats(prop)

ds_long %>% filter(age >= 8) %>% group_by(position) %>% get_summary_stats(prop)

# By age

ds_long %>% drop_na(prop) %>% 
  ggplot() + facet_wrap("position") + 
  stat_summary(aes(x = age, y = prop, group = id_uni, color = position)) + 
  geom_smooth(aes(x = age, y = prop, color = position), method = "lm", se = FALSE) + 
  scale_x_continuous(name = "Age (mo)", breaks = seq(3, 15, 3), limits = c(3,15))

ds_sum <- ds_long %>% group_by(id_uni, age, position) %>% get_summary_stats(prop)
summary(lm(mean ~ age, data = filter(ds_sum, position == "held_time")))
summary(lm(mean ~ age, data = filter(ds_sum, position == "supine_time")))
summary(lm(mean ~ age, data = filter(ds_sum, position == "sit_time")))
summary(lm(mean ~ age, data = filter(ds_sum, position == "prone_time")))
summary(lm(mean ~ age, data = filter(ds_sum, position == "upright_time")))

#Dominant body position? 

ds_long %>% 
  group_by(id_uni, age, clock_time_start_2) %>%
  filter(nap_time == 0) %>% 
  summarize(dominant = max(prop, na.rm = T)) %>% 
  filter(dominant > 0) %>% ungroup %>% 
  mutate(age_group = factor(ifelse(age < 8, 0, 1), labels = c("Younger","Older"))) %>% 
  drop_na(dominant, age_group) -> dominance

lmPerm::lmp(dominant ~ age_group, data = dominance) %>% summary

dominance %>% group_by(age_group) %>% get_summary_stats(dominant)

theme_update(text = element_text(size = 20),
             axis.text.x = element_text(size = 20), axis.title.x = element_text(size = 24),
             axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 24)) 
ggplot(dominance, aes(y = dominant, x = age_group)) + 
  geom_boxplot() + xlab("") + ylim(0,1) + 
  ylab("Proportion of Dominant Position")
ggsave(here("analysis-lena-imu", "figures", "dominance.png"))

ggplot(dominance, aes(x = dominant, color = age_group)) + 
  geom_density() 

ggplot(dominance, aes(x = dominant)) + 
  geom_histogram(position = "dodge") + 
  facet_wrap("age_group", scales = "free_y")


# Export for WPA

wpa <- ds %>% filter(age >= 8, nap_time == 0)
wpa <- wpa %>% select(id_uni, age, sit_time:upright_time, adult_word_cnt, child_utt_cnt, conv_turn_count, child_cry_scnds)

wpa %>% cor_mat(-id_uni, -age) %>% cor_mark_significant()

wpa %>% write_csv("analysis-lena-imu/wpa_lena_pos.csv")




