rm(list = ls())
library(tidyverse)
library(here)
library(lubridate)
library(patchwork)
i_am(".here")

include <- read_csv(here("code","analysis-lena-imu","ppt_include.csv"))

ds <- pmap_dfr(include, ~ read_csv(here("data",..1, ..2,"synced_data","lena_imu_infant.csv")) %>% add_column(age = ..3))

write_csv(ds, here("code","analysis-lena-imu","lena-imu-compiled.csv"))

