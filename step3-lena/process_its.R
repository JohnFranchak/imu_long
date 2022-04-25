#devtools::install_github("htanderson/ITSbin", dependencies = TRUE)
library(here)
library(ITSbin)
library(tidyverse)
library(glue)
i_am(".here")

id <- 107
session <- 2
who <- "infant"

setwd(here("data",id,session,"lena"))

check_multiday(
  ITS.dir = "its",
  CSV.dir = "output",
  time.zone = "America/Los_Angeles")

ITS_to_seconds(
  ITS.dir = "its",
  CSV.dir = "output",
  time.zone = "America/Los_Angeles")

bin_seconds(
  seconds.dir = "output/seconds",
  output.dir = "output/bin",
  bin.to.mins = 10,
  roll.by.mins = 5,
  align.rows = "midnight")

i_am(".here")
