process_its <- function(id, session) {

#devtools::install_github("htanderson/ITSbin", dependencies = TRUE)
require(here)
require(ITSbin)
require(tidyverse)
require(glue)
i_am(".here")

# id <- 107
# session <- 2

setwd(here("data",id,session,"lena"))
if (!dir.exists("output")) {
  dir.create("output")
}

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

}
