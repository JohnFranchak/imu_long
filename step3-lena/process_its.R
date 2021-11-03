#devtools::install_github("htanderson/ITSbin", dependencies = TRUE)
library(here)
library(ITSbin)
library(tidyverse)
library(glue)
i_am(".here")

id <- 99
session <- 8
who <- "infant"

lena_dir <- glue("data/{id}/{session}/lena")
lena_dir_output <- glue("data/{id}/{session}/lena/output")

check_multiday(
  ITS.dir = lena_dir,
  CSV.dir = lena_dir_output,
  time.zone = "America/Los_Angeles")

ITS_to_seconds(
  ITS.dir = lena_dir,
  CSV.dir = lena_dir_output,
  time.zone = "America/Los_Angeles")

bin_seconds(
  seconds.dir = glue("data/{id}/{session}/lena/output/seconds"),
  output.dir = glue("data/{id}/{session}/lena/output/bin"),
  bin.to.mins = 10,
  roll.by.mins = 5,
  align.rows = "midnight")
