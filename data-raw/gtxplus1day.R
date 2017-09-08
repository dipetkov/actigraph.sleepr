
library("dplyr")
library("actigraph.sleepr")

gtxplus1day <- read_agd(file.path("data-raw", "actigraph",
                                  "GT3XPlus-RawData(1-20)",
                                  "GT3XPlus-RawData-Day01-10sec.agd")) %>%
  select(timestamp, starts_with("axis"))

devtools::use_data(gtxplus1day, overwrite = TRUE)
