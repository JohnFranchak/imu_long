library(tidyverse)
library(rstatix)
library(here)
i_am(".here")

pt2_files <- list.files(here("data"), pattern = "position_agreement_infant_pt2.csv", recursive = T, full.names = T)

ds <- read_csv(pt2_files, id = "file")
write_csv(ds, here("code", "analysis-part2", "compiled_agreement.csv"))

# LOCAL
# ds <- read_csv(here("analysis-part2", "compiled_agreement.csv"))

# OVERALL, UGH
ds_filt <- ds %>% filter(nap_period == 0 & exclude_period == 0 & !is.na(code) & !is.na(pos))
ds_filt <- ds %>% filter(exclude_period == 0 & !is.na(code) & !is.na(pos))

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

# 10 min bins
ds_filt <- ds_filt %>% group_by(file) %>% mutate(sample = row_number(), 
                                                 bin = floor(sample/300)) %>% 
  add_count(file, bin) %>% ungroup() %>% filter(n > 240)

model <- ds_filt %>% count(file, bin, pos) %>% add_count(file, bin, wt = n, name = "total") %>% mutate(prop_model = n/total)
human <- ds_filt %>% count(file, bin, code) %>% add_count(file, bin, wt = n, name = "total") %>% mutate(prop_human = n/total)

agree <- full_join(select(model, file, pos, prop_model, bin), 
                   select(human, file, code, prop_human, bin), 
                   by = c("file" = "file", "pos" = "code", "bin" = "bin"))
agree <- agree %>% mutate(prop_human = ifelse(is.na(prop_human), 0, prop_human),
                          prop_model = ifelse(is.na(prop_model), 0, prop_model))
agree <- complete(agree, nesting(file, bin), pos, fill = list(prop_human = 0, prop_model = 0))

cor_test(agree, vars = c("prop_model", "prop_human"))

ggplot(agree, aes(x = prop_human, y = prop_model)) + facet_wrap("pos") + 
  geom_point() + geom_smooth(method = "lm") + xlim(0,1) + ylim(0,1)

(ind_corr <- agree %>% group_by(file) %>% cor_test(vars = c("prop_model", "prop_human")) %>% arrange(cor))
# ind_corr %>% write_csv(here("code","analysis-part2", "agree_by_ppt.csv"))

# check individual agree cf pt 1 accuracy

split_model_acc <- function(file) {
  file_acc <- str_replace(file, pattern = "position_agreement_infant_pt2.csv", 
                          replacement = "model_performance_infant_split.RData")
  load(file_acc)
  res$`Balanced Accuracy`
}

ind_corr <- ind_corr %>% mutate(pt1_acc = split_model_acc(file))

acc_pt1 <- map_dbl(ind_corr$file, split_model_acc)
