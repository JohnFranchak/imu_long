library(tidyverse)
library(rstatix)
library(here)
i_am(".here")

type <- "position"

pt2_files <- list.files(here("data"), pattern = str_glue("{type}_agreement_infant_pt2.csv", recursive = T, full.names = T))

ds <- read_csv(pt2_files, id = "file")
ds <- ds %>% mutate(file = str_remove(file, "Z:/study_imu_long/data/"),
                    file = str_remove(file, "/synced_data/sitting_agreement_infant_pt2.csv"),
                    file = str_remove(file, "/synced_data/position_agreement_infant_pt2.csv"),
                    file = str_remove(file, "/synced_data/split_agreement_infant_pt2.csv"))
write_csv(ds, here("code", "analysis-part2", str_glue("compiled_agreement_{type}.csv")))

# LOCAL
# ds <- read_csv(here("analysis-part2", "compiled_agreement.csv"))

# OVERALL, UGH
ds_filt <- ds %>% filter(nap_period == 0 & exclude_period == 0 & !is.na(code) & !is.na(pos))
# ds_filt <- ds %>% filter(exclude_period == 0 & !is.na(code) & !is.na(pos))

model <- ds_filt %>% count(file, pos) %>% add_count(file, wt = n, name = "total") %>% mutate(prop_model = n/total)
human <- ds_filt %>% count(file, code) %>% add_count(file, wt = n, name = "total") %>% mutate(prop_human = n/total)

agree <- full_join(select(model, file, pos, prop_model), 
                   select(human, file, code, prop_human), 
                   by = c("file" = "file", "pos" = "code"))
agree <- agree %>% mutate(prop_human = ifelse(is.na(prop_human), 0, prop_human),
                          prop_model = ifelse(is.na(prop_model), 0, prop_model))
agree <- complete(agree, file, pos, fill = list(prop_human = 0, prop_model = 0))

cor_test(agree, vars = c("prop_model", "prop_human"))

ggplot(agree, aes(x = prop_human, y = prop_model)) + facet_wrap("pos") + 
  geom_point() + geom_smooth(method = "lm") + xlim(0,1) + ylim(0,1)

# ggplot(filter(agree, pos == "Sitting"), aes(x = prop_human, y = prop_model)) +
#   geom_point() + geom_smooth(method = "lm") + xlim(0,1) + ylim(0,1)

# 10 min bins
ds_filt <- ds %>% filter(nap_period == 0 & exclude_period == 0 & !is.na(code) & !is.na(pos))
ds_filt <- ds_filt %>% group_by(file) %>% mutate(sample = row_number(), 
                                                 bin = floor(sample/300)) %>% 
  add_count(file, bin) %>% ungroup() %>% filter(n > 200) %>% group_by(file) %>% mutate(num_bins = length(unique(bin))) %>% ungroup()

model <- ds_filt %>% count(file, bin, num_bins, pos) %>% add_count(file, bin,num_bins,  wt = n, name = "total") %>% mutate(prop_model = n/total)
human <- ds_filt %>% count(file, bin, num_bins,  code) %>% add_count(file, bin, num_bins, wt = n, name = "total") %>% mutate(prop_human = n/total)

agree <- full_join(select(model, file, pos, prop_model, bin, num_bins), 
                   select(human, file, code, prop_human, bin, num_bins), 
                   by = c("file" = "file", "pos" = "code", "bin" = "bin", "num_bins" = "num_bins"))
agree <- agree %>% mutate(prop_human = ifelse(is.na(prop_human), 0, prop_human),
                          prop_model = ifelse(is.na(prop_model), 0, prop_model))
agree <- complete(agree, nesting(file, bin, num_bins), pos, fill = list(prop_human = 0, prop_model = 0))

#remove worst performers
# agree <- agree %>% filter(file != "106/1") %>% filter(file != "107/1")

cor_test(agree, vars = c("prop_model", "prop_human"))
agree %>% group_by(pos) %>% cor_test(vars = c("prop_model", "prop_human")) %>% arrange(cor)

ggplot(filter(agree), aes(x = prop_human, y = prop_model, color = file)) + facet_wrap("pos") + 
  geom_point() + geom_smooth(method = "lm", se = F) + xlim(0,1) + ylim(0,1) + 
  theme(legend.position = "bottom")

(ind_corr <- agree %>% group_by(file) %>% cor_test(vars = c("prop_model", "prop_human")) %>% arrange(cor))
# ind_corr %>% write_csv(here("code","analysis-part2", "agree_by_ppt.csv"))

ggplot(filter(agree), aes(x = prop_human, y = prop_model)) + facet_wrap("file") + 
  geom_point() + geom_smooth(method = "lm", se = F) + xlim(0,1) + ylim(0,1) + 
  theme(legend.position = "bottom")

# 5 min bins
ds_filt <- ds %>% filter(nap_period == 0 & exclude_period == 0 & !is.na(code) & !is.na(pos))
ds_filt <- ds_filt %>% group_by(file) %>% mutate(sample = row_number(), 
                                                 bin = floor(sample/150)) %>% 
  add_count(file, bin) %>% ungroup() %>% filter(n > 130) %>% group_by(file) %>% mutate(num_bins = length(unique(bin))) %>% ungroup()

model <- ds_filt %>% count(file, bin, num_bins, pos) %>% add_count(file, bin,num_bins,  wt = n, name = "total") %>% mutate(prop_model = n/total)
human <- ds_filt %>% count(file, bin, num_bins,  code) %>% add_count(file, bin, num_bins, wt = n, name = "total") %>% mutate(prop_human = n/total)

agree <- full_join(select(model, file, pos, prop_model, bin, num_bins), 
                   select(human, file, code, prop_human, bin, num_bins), 
                   by = c("file" = "file", "pos" = "code", "bin" = "bin", "num_bins" = "num_bins"))
agree <- agree %>% mutate(prop_human = ifelse(is.na(prop_human), 0, prop_human),
                          prop_model = ifelse(is.na(prop_model), 0, prop_model))
agree <- complete(agree, nesting(file, bin, num_bins), pos, fill = list(prop_human = 0, prop_model = 0))

#remove worst performers
# agree <- agree %>% filter(file != "106/1") %>% filter(file != "107/1")

cor_test(agree, vars = c("prop_model", "prop_human"))
agree %>% group_by(pos) %>% cor_test(vars = c("prop_model", "prop_human")) %>% arrange(cor)

ggplot(filter(agree, num_bins > 2), aes(x = prop_human, y = prop_model, color = file)) + facet_wrap("pos") + 
  geom_point() + geom_smooth(method = "lm", se = F) + xlim(0,1) + ylim(0,1) + 
  theme(legend.position = "bottom")

ggplot(filter(agree, pos == "Sitting", num_bins > 2), aes(x = prop_human, y = prop_model, color = file)) + facet_wrap("file") + 
  geom_point() + geom_smooth(method = "lm", se = F) + xlim(0,1) + ylim(0,1) + 
  theme(legend.position = "bottom")

# check individual agree cf pt 1 accuracy - IT IS NOT

ind_corr <- ind_corr %>% 
  filter(file != "110/4") %>% 
  filter(file != "116/1")

split_model_acc <- function(file) {
  file_acc <- str_replace(file, pattern = "position_agreement_infant_pt2.csv", 
                          replacement = "model_performance_infant_split.RData")
  file_acc <- str_remove(file_acc, pattern = "Z:/study_imu_long/data/")
  print(file_acc)
  load(here("data",file_acc))
  res$`Balanced Accuracy`
}

acc_pt1 <- map_dbl(ind_corr$file, split_model_acc)
ind_corr$acc_pt1 <- acc_pt1

cor_test(ind_corr, vars = c("cor", "acc_pt1"))
