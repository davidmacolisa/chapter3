#======================================================================================================================#
### WD
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Libraries
#======================================================================================================================#
library(tidyverse)

#======================================================================================================================#
### NBER CES DATA
#======================================================================================================================#
filepath <- "./Data_PhD/US/NBER/nberces5818v1_n2012.csv"
nber_ces <- read_csv(file = filepath, show_col_types = F) %>%
  filter(year >= 2011 & year <= 2017) %>%
  data.frame() %>%
  select(c(naics, year:plant, tfp4, tfp5))

nber_ces[nber_ces$year == 2017, ]$cap <- mean(nber_ces$cap, na.rm = T)
nber_ces[nber_ces$year == 2017, ]$equip <- mean(nber_ces$equip, na.rm = T)
nber_ces[nber_ces$year == 2017, ]$plant <- mean(nber_ces$plant, na.rm = T)
nber_ces[nber_ces$year == 2017, ]$tfp4 <- mean(nber_ces$tfp4, na.rm = T)
nber_ces[nber_ces$year == 2017, ]$tfp5 <- mean(nber_ces$tfp5, na.rm = T)
sum(is.na(nber_ces))
n_distinct(nber_ces$naics)
write_rds(x = nber_ces, file = "./Data_PhD/US/NBER/nber_ces.rds", compress = "xz")
#======================================================================================================================#