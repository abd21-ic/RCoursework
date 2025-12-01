rm(list = ls())

#install.packages("tidyverse")

require(tidyverse)
require(camtrapR)
require(overlap)

# reading all tables and combining them
list.of.files <- list.files(
  "../data/camera_trap",
  pattern = "\\.csv$",
  full.names = TRUE
)

# checking number of columns
colinfo <- lapply(list.of.files, function(f) {
  data.frame(
    file = f,
    ncol = ncol(read.csv(f)),
    colnames = I(list(names(read.csv(f))))
  )
})

colinfo <- do.call(rbind, colinfo)
colinfo

# force reading csv with station as character
dfs <- lapply(list.of.files, function(f) {
  read.csv(f, colClasses = c(Station = "factor"))
})

sp_table <- bind_rows(dfs)

write.csv(sp_table, "combined_table.csv")

# checking which rows have bad ymd format
library(lubridate)

bad <- which(is.na(ymd_hms(sp_table$DateTimeOriginal)))
sp_table[bad, "DateTimeOriginal"]

# fixing bad rows
sp_table$DateTime_fixed <- parse_date_time(
  sp_table$DateTimeOriginal,
  orders = c("ymd HMS", "ymd HM", "ymdHMS", "ymdHM",
             "dmy HMS", "dmy HM",
             "mdy HMS", "mdy HM",
             "Ymd HMS", "Ymd HM",
             "Y-m-d H:M:S", "Y-m-d H:M"),
  tz = "UTC")

sp_table$DateTimeOriginal <- sp_table$DateTime_fixed
sp_table$DateTime_fixed <- NULL

# fixing mis-spelling of Species names
fix_species <- c(
  "Sciurus carolinensis" = "Sciurus_carolinensis",
  "Muntiacus reevesi" = "Muntiacus_reevesi",
  "Capreolus capreolus" = "Capreolus_capreolus"
)

sp_table <- sp_table %>%
  mutate(Species = recode(Species, !!!fix_species)) %>% 
  filter(Species != "Homo_sapiens") # removing human detections

