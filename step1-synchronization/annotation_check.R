library(tidyverse)
library(here)
i_am(".here")

files_to_check <- list.files(here("data"), pattern = "biostamp_annotations.csv", recursive = TRUE, full.names = TRUE)

anno <- read_csv(files_to_check, id = "file") %>% mutate(across(where(is.numeric), as.character))

                                                         