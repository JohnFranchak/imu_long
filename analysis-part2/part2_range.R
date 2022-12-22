library(tidyverse)
library(av)
library(tidyvyur)
library(lubridate)
library(here)
source(here("code","analysis-part2","av_info.R"))
i_am(".here")


id <- 102
session <- 1
who <- "infant"

av_info <- av_info(id, session)
