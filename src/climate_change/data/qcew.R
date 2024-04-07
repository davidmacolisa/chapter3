#======================================================================================================================#
### Packages
#======================================================================================================================#
library(tidyverse)
library(stringr)
library(statar)
library(usgeogr)
#======================================================================================================================#
### Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading QCEW Data
#======================================================================================================================#
# filepath2005 <- "./Data_PhD/US/BLS/qcew/A/2005.annual.singlefile.csv"
# filepath2006 <- "./Data_PhD/US/BLS/qcew/A/2006.annual.singlefile.csv"
# filepath2007 <- "./Data_PhD/US/BLS/qcew/A/2007.annual.singlefile.csv"
# filepath2008 <- "./Data_PhD/US/BLS/qcew/A/2008.annual.singlefile.csv"
# filepath2009 <- "./Data_PhD/US/BLS/qcew/A/2009.annual.singlefile.csv"
# filepath2010 <- "./Data_PhD/US/BLS/qcew/A/2010.annual.singlefile.csv"
# filepath2011 <- "./Data_PhD/US/BLS/qcew/A/2011.annual.singlefile.csv"
# filepath2012 <- "./Data_PhD/US/BLS/qcew/A/2012.annual.singlefile.csv"
# filepath2013 <- "./Data_PhD/US/BLS/qcew/A/2013.annual.singlefile.csv"
# filepath2014 <- "./Data_PhD/US/BLS/qcew/A/2014.annual.singlefile.csv"
# filepath2015 <- "./Data_PhD/US/BLS/qcew/A/2015.annual.singlefile.csv"
# filepath2016 <- "./Data_PhD/US/BLS/qcew/A/2016.annual.singlefile.csv"
# filepath2017 <- "./Data_PhD/US/BLS/qcew/A/2017.annual.singlefile.csv"
# filepath2018 <- "./Data_PhD/US/BLS/qcew/A/2018.annual.singlefile.csv"
# filepath2019 <- "./Data_PhD/US/BLS/qcew/A/2019.annual.singlefile.csv"
# filepath2020 <- "./Data_PhD/US/BLS/qcew/A/2020.annual.singlefile.csv"
# filepath2021 <- "./Data_PhD/US/BLS/qcew/A/2021.annual.singlefile.csv"
# filepath2022 <- "./Data_PhD/US/BLS/qcew/A/2022.annual.singlefile.csv"
#
# qcew05 <- read.csv(file = filepath2005, header = T) %>% lapply(., as.character)
# qcew06 <- read.csv(file = filepath2006, header = T) %>% lapply(., as.character)
# qcew07 <- read.csv(file = filepath2007, header = T) %>% lapply(., as.character)
# qcew08 <- read.csv(file = filepath2008, header = T) %>% lapply(., as.character)
# qcew09 <- read.csv(file = filepath2009, header = T) %>% lapply(., as.character)
# qcew10 <- read.csv(file = filepath2010, header = T) %>% lapply(., as.character)
# qcew11 <- read.csv(file = filepath2011, header = T) %>% lapply(., as.character)
# qcew12 <- read.csv(file = filepath2012, header = T) %>% lapply(., as.character)
# qcew13 <- read.csv(file = filepath2013, header = T) %>% lapply(., as.character)
# qcew14 <- read.csv(file = filepath2014, header = T) %>% lapply(., as.character)
# qcew15 <- read.csv(file = filepath2015, header = T) %>% lapply(., as.character)
# qcew16 <- read.csv(file = filepath2016, header = T) %>% lapply(., as.character)
# qcew17 <- read.csv(file = filepath2017, header = T) %>% lapply(., as.character)
# qcew18 <- read.csv(file = filepath2018, header = T) %>% lapply(., as.character)
# qcew19 <- read.csv(file = filepath2019, header = T) %>% lapply(., as.character)
# qcew20 <- read.csv(file = filepath2020, header = T) %>% lapply(., as.character)
# qcew21 <- read.csv(file = filepath2021, header = T) %>% lapply(., as.character)
# qcew22 <- read.csv(file = filepath2022, header = T) %>% lapply(., as.character)

# qcew <- bind_rows(
#   qcew05, qcew06, qcew07, qcew08,
#   qcew09, qcew10, qcew11, qcew12, qcew13, qcew14,
#   qcew15, qcew16, qcew17, qcew18, qcew19,
#   qcew20, qcew21, qcew22
# ) %>% data.frame()
#
# sort(unique(qcew$year))
# write_rds(qcew, file = "./Data_PhD/US/BLS/qcew.rds", compress = "xz")
#======================================================================================================================#