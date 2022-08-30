library(tidyverse)
library(here)
library(glue)
i_am(".here")

ids <- list.dirs(here("data"), recursive = F) %>% discard(str_detect(., pattern = "_template"))
sessions_dir <- map(ids, ~list.dirs(.x, recursive = F)) %>% flatten_chr
complate_paperwork <- map(sessions_dir, ~ length(list.files(str_glue("{.x}/scanned_paperwork"))))


load(here("data",id,session, "synced_data", glue("mot_features_{who}.RData")))
