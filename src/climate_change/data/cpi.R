#======================================================================================================================#
### WD
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Libraries
#======================================================================================================================#
library(tidyverse)
library(data.table)

#======================================================================================================================#
### BLS-CPI-All Urban Consumers
#======================================================================================================================#
current <- fread(file = "./Data_PhD/US/BLS/cpi/cu.data.0.Current.txt", header = T, fill = T, check.names = T, sep = "\t", quote = "")
series <- fread(file = "./Data_PhD/US/BLS/cpi/cu.series.txt", header = T, fill = T, check.names = T, sep = "\t", quote = "")

cpi <- series %>%
  left_join(
    y = current,
    by = "series_id"
  ) %>%
  # keep only annual avgs
  filter(period == "S03" &
           area_code == "0000" &
           (year >= 2011 &
             year <= 2017) &
           series_title == "All items in U.S. city average, all urban consumers, not seasonally adjusted") %>%
  select(c(year, value)) %>%
  rename(cpi = value) %>%
  data.frame() %>%
  write_rds(file = "./Data_PhD/US/BLS/cpi/cpi.rds", compress = "xz")
sort(unique(cpi$series_title))