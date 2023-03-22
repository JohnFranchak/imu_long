args <- commandArgs()
print(args)

id <- args[6]
session <- args[7]

library(here)
library(stringr)
i_am(".here")

fs <- list.files(here("temp_imu"), pattern = paste0(".*",as.character(id),".*zip"), full.names = T)
for (f in fs) {
  print(f)
  side <- ifelse(str_detect(f, "left"), "left", "right")
  part <- ifelse(str_detect(f, "foot"), "ankle", "hip")
  exdir <- here("data",id,session, "imu", str_glue("{side}_{part}"))
  unzip(zipfile = f, exdir = exdir)
}



