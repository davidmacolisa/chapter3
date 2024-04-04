#======================================================#
### API AND DATA
#======================================================#
### BLS Employment and Wage data -
# Quarterly Census of Employment and Wages
#======================================================#
## Packages
#======================================================#
# library(ndjson)
# library(jsonlite)
# library(httr)
# library(rjson)
# library(remotes)
# library(devtools)
# install_github("mikeasilva/blsAPI")
# library(blsAPI)
library(tidyverse)
library(readr)
library(statar)
library(data.table)
#======================================================#
### Working Directory
#======================================================#
setwd("C:/Users/Davidmac.Ekeocha/OneDrive/Documents/ULMS/PhD/")
#======================================================#
### Loaidng data
#======================================================#
filepath <- "Data_PhD/US/BLS/qcew/2011_all_enb/state/st01al11.enb"
filepath1 <- "Data_PhD/US/BLS/employment_hours_earnings_series/sm.data.0.Current.txt"
filepath2 <- "Data_PhD/US/BLS/employment_hours_earnings_series/sm.data.1.AllData.txt"
areapath <- "Data_PhD/US/BLS/employment_hours_earnings_series/sm.area.txt"
data.type <- "Data_PhD/US/BLS/employment_hours_earnings_series/sm.data_type.txt"
industry.type <- "Data_PhD/US/BLS/employment_hours_earnings_series/sm.industry.txt"
period <- "Data_PhD/US/BLS/employment_hours_earnings_series/sm.period.txt"
data.series <- "Data_PhD/US/BLS/employment_hours_earnings_series/sm.series.txt"
statepath <- "Data_PhD/US/BLS/employment_hours_earnings_series/sm.state.txt"
supersector.path <- "Data_PhD/US/BLS/employment_hours_earnings_series/sm.supersector.txt"
footnote.path <- "Data_PhD/US/BLS/employment_hours_earnings_series/sm.footnote.txt"

data <- read.fwf(file = filepath, widths = 51, header = T)
data1 <- fread(file = filepath1, header = T, fill = T, sep = "auto", quote = "\"") %>% filter(period == "M13") # annual avgs
data2 <- fread(file = filepath2, header = T, fill = T, sep = "auto", quote = "\"") %>% filter(period == "M13") # annual avgs
area <- fread(file = areapath, header = T, fill = T, sep = "auto", quote = "\"") %>% filter(area_code != 0)
datatype <- fread(file = data.type, header = T, fill = T, sep = "auto", quote = "\"")
industry <- fread(file = industry.type, header = T, fill = T, sep = "auto", quote = "\"")
dates <- fread(file = period, header = T, fill = T, sep = "auto", quote = "\"")
series <- fread(file = data.series, header = T, fill = T, sep = "auto", quote = "\"")
states <- fread(file = statepath, header = T, fill = T, sep = "auto", quote = "\"")
supersector <- fread(file = supersector.path, header = T, fill = T, sep = "auto", quote = "\"")
footnote <- fread(file = footnote.path, header = T, fill = T, sep = "auto", quote = "\"") %>% rename(footnote_codes = footnote_code)
#======================================================#
### Cleaning and merging data files
#======================================================#
empwage <- merge(x = series, y = area)
empwage <- merge(x = empwage, y = datatype, by = "data_type_code")
empwage <- merge(x = empwage, y = industry, by = "industry_code")
empwage <- merge(x = empwage, y = states, by = "state_code")
empwage <- merge(x = empwage, y = data2, by = "series_id")

glimpse(empwage)
empwage <- empwage %>%
  select(c(
    series_id, state_code, state_name, area_code, area_name, year, value, data_type_code, data_type_text,
    industry_code, industry_name, benchmark_year, begin_year, end_year)) %>%
  filter(industry_name != "Total Nonfarm") %>%
  filter(industry_name != "Total Private") %>%
  filter(industry_name != "Goods Producing") %>%
  filter(industry_name != "Service-Providing") %>%
  filter(industry_name != "Private Service Providing") %>%
  filter(industry_name != "Mining and Logging")
sort(unique(empwage$year))
#======================================================#
### Selecting the states - 50 states || two states
#======================================================#
sort(unique(empwage$state_name))
empwage <- empwage %>%
  filter(state_name != "District of Columbia") %>%
  filter(state_name != "Puerto Rico") %>%
  filter(state_name == "Illinois" | state_name == "Indiana")
sort(unique(empwage$data_type_text))
#======================================================#
### Pivoting wider for the variables
#======================================================#
empwage1 <- empwage %>% pivot_wider(names_from = c(data_type_code, data_type_text), values_from = value, names_sep = "_")
#======================================================#
### Loading Data - TRI Form R
#======================================================#
filepath <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/triR.rds"
tri <- readRDS(file = filepath) %>%
  filter(facility.state == "CA" | facility.state == "OR") %>%
  filter(year >= 2006)
tri <-  tri %>% select(-facility.state.code)

wageRD <- merge(x = tri, y = ipc.merge, by = c("facility.state", "year"))
wageRD <- merge(x = tri, y = bdsm, by = "naics")


#======================================================#
### National Compensation Survey
### Loaidng data
#======================================================#
filepath <- "Data_PhD/US/BLS/national compensation survey/nw.data.1.AllData.txt"
areapath <- "Data_PhD/US/BLS/national compensation survey/nw.starea.txt"
data.type <- "Data_PhD/US/BLS/national compensation survey/nw.datatype_id.txt"
industry.type <- "Data_PhD/US/BLS/national compensation survey/nw.industry.txt"
level.type <- "Data_PhD/US/BLS/national compensation survey/nw.level.txt"
occupation.path <- "Data_PhD/US/BLS/national compensation survey/nw.occupation.txt"
ownership.path <- "Data_PhD/US/BLS/national compensation survey/nw.ownership.txt"
# statepath <- "Data_PhD/US/BLS/national compensation survey/sm.state.txt"
series.path <- "Data_PhD/US/BLS/national compensation survey/nw.series.txt"
subcellid.path <- "Data_PhD/US/BLS/national compensation survey/nw.subcell_id.txt"
footnote.path <- "Data_PhD/US/BLS/national compensation survey/nw.footnote.txt"

data <- fread(file = filepath, header = T, fill = T, sep = "auto", quote = "\"")
area <- fread(file = areapath, header = T, fill = T, sep = "auto", quote = "\"") %>% filter(state_code != 0)
datatype <- fread(file = data.type, header = T, fill = T, sep = "auto", quote = "\"")
industry <- fread(file = industry.type, header = T, fill = T, sep = "auto", quote = "\"") %>% filter(industry_text != "All workers")
# dates <- fread(file = period, header = T, fill = T, sep = "auto", quote = "\"")
series <- fread(file = series.path, header = T, fill = T, sep = "auto", quote = "\"") %>% filter(industry_code != "000000")
# states <- fread(file = statepath, header = T, fill = T, sep = "auto", quote = "\"")
subcellid <- fread(file = subcellid.path, header = T, fill = T, sep = "auto", quote = "\"") %>% filter(subcell_id_text != "All workers")
footnote <- fread(file = footnote.path, header = T, fill = T, sep = "auto", quote = "\"")
#======================================================#
### Cleaning and merging data files
#======================================================#
empwage <- merge(x = series, y = industry)
empwage <- merge(x = series, y = data)
empwage <- merge(x = empwage, y = datatype, by = "datatype_id_code")
empwage <- merge(x = empwage, y = industry, by = "industry_code")
empwage <- merge(x = empwage, y = industry)
empwage <- merge(x = empwage, y = states, by = "state_code")
# empwage <- merge(x = empwage, y = data, by = "series_id")

glimpse(empwage)
empwage <- empwage %>%
  select(c(
    series_id, state_code, state_name, area_code, area_name, year, value, data_type_code, data_type_text,
    industry_code, industry_name, benchmark_year, begin_year, end_year)) %>%
  filter(industry_name != "Total Nonfarm") %>%
  filter(industry_name != "Total Private") %>%
  filter(industry_name != "Goods Producing") %>%
  filter(industry_name != "Service-Providing") %>%
  filter(industry_name != "Private Service Providing") %>%
  filter(industry_name != "Mining and Logging")
sort(unique(empwage$year))
#======================================================#
### Selecting the states - 50 states || two states
#======================================================#
sort(unique(empwage$state_name))
empwage <- empwage %>%
  filter(state_name != "District of Columbia") %>%
  filter(state_name != "Puerto Rico") %>%
  filter(state_name == "Illinois" | state_name == "Indiana")
sort(unique(empwage$data_type_text))
#======================================================#
### Pivoting wider for the variables
#======================================================#
empwage1 <- empwage %>% pivot_wider(names_from = c(data_type_code, data_type_text), values_from = value, names_sep = "_")
#======================================================#
### Slice out empwage data for the variables
### merging
#======================================================#
sort(unique(empwage$data_type_text))
allemployees <- empwage %>%
  filter(data_type_text == "All Employees, In Thousands") %>%
  rename(allemployees = value) %>%
  select(state_name, area_name, year, allemployees)

avgweeklyhours.allemployees <- empwage %>%
  filter(data_type_text == "Average Weekly Earnings of Production Employees, In Dollars") %>%
  rename(avgweeklyhours.allemployees = value) %>%
  select(state_name, area_name, year, avgweeklyhours.allemployees)

#======================================================#
### QCEW data
### Loaidng data
#======================================================#
filepath <- "Data_PhD/US/BLS/qcew/2011_all_enb/state/allsta11.txt"
data <- fread(file = filepath, header = T, fill = T, sep = " ", quote = "\"")