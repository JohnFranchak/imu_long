#devtools::install_github("JohnFranchak/tidyvyur")

library(tidyverse)
library(tidyvyur)
library(lubridate)
library(here)
i_am(".here")

ids <- list.dirs(here("data"), recursive = F) %>% purrr::discard(str_detect(., pattern = "_template")) %>% purrr::discard(str_detect(., pattern = "_RAs"))
sessions_dir <- map(ids, ~list.dirs(.x, recursive = F)) %>% flatten_chr

#check coding
opf_files <- map(sessions_dir, ~ list.files(str_glue("{.x}/coding"), "*pt1.opf", full.names = T))
opf_files <- unlist(opf_files)

return_sync <- function(opf_file) {
  if (str_detect(opf_file, ".opf")) {
    tmp_opf <- read_opf(opf_file)[['sync']] 
    tmp_opf$file <- unlist(opf_file)
    if(is.logical(tmp_opf$offset)) {
      return(NULL)
    } else {
      return(tmp_opf)
    }
  } else {
    return(NULL)
  }
}

sync_data <- map_dfr(opf_files, return_sync)
