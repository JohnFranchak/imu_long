library(here)
library(tidyverse)
i_am(".here")

id <- 105
session <- 1

fs <- list.files(here("temp_imu"), pattern = paste0(".*",as.character(id),".*zip"), full.names = T)
for (f in fs) {
  side <- ifelse(str_detect(f, "left"), "left", "right")
  part <- ifelse(str_detect(f, "foot"), "ankle", "hip")
  exdir <- here("data",id,session, "imu", str_glue("{side}_{part}"))
  unzip(zipfile = f, exdir = exdir)
}



