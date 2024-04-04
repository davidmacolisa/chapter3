#======================================================#
#### DOL - INDUSTRY PRODUCTIVITY COST -  bulk download
#======================================================#
### Packages
#======================================================#
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
filepath <- "Data_PhD/US/BLS/ipc/ip.data.1.AllData.txt"
areapath <- "Data_PhD/US/BLS/ipc/ip.area.txt"
industry.type <- "Data_PhD/US/BLS/ipc/ip.industry.txt"
measure.type <- "Data_PhD/US/BLS/ipc/ip.measure.txt"
sector.type <- "Data_PhD/US/BLS/ipc/ip.sector.txt"
data.series <- "Data_PhD/US/BLS/ipc/ip.series.txt"
iptype.path <- "Data_PhD/US/BLS/ipc/ip.type.txt"
duration.path <- "Data_PhD/US/BLS/ipc/ip.duration.txt"

data <- fread(file = filepath, header = T, fill = T, sep = "auto", quote = "\"") %>% filter(year >= 2005) # annual avgs
area <- fread(file = areapath, header = T, fill = T, sep = "auto", quote = "\"") %>% filter(area_code != 0)
industry <- fread(file = industry.type, header = T, fill = T, sep = "auto", quote = "\"")
measure <- fread(file = measure.type, header = T, fill = T, sep = "auto", quote = "\"")
series <- fread(file = data.series, header = T, fill = T, sep = "\t", quote = "\"") %>% filter(area_code != 0)
iptype <- fread(file = iptype.path, header = T, fill = T, sep = "auto", quote = "\"")
duration <- fread(file = duration.path, header = T, fill = T, sep = "auto", quote = "\"")
sector <- fread(file = sector.type, header = T, fill = T, sep = "auto", quote = "\"")
#======================================================#
### Cleaning and merging data files
#======================================================#
ipc <- merge(x = data, y = series)
ipc <- merge(x = ipc, y = area, by = "area_code")
ipc <- merge(x = ipc, y = industry, by = "industry_code")
ipc <- merge(x = ipc, y = measure, by = "measure_code")
ipc <- merge(x = ipc, y = iptype, by = "type_code")
ipc <- merge(x = ipc, y = duration, by = "duration_code")
ipc <- merge(x = ipc, y = sector, by = "sector_code")
glimpse(ipc)

ipc <- ipc %>%
  select(c(
    series_id, area_code, area_text, series_title, year, value, measure_text, type_text, measure_text,
    duration_text, industry_text,sector_text, begin_year, end_year))
sort(unique(ipc$year))
#======================================================#
### Selecting the states - 50 states || two states
#======================================================#
sort(unique(ipc$area_text))
ipc <- ipc %>%
  filter(area_text != "District of Columbia") %>%
  # filter(state_name != "Puerto Rico") %>%
  filter(area_text == "California" | area_text == "Oregon") %>%
  filter(type_text != "Index")
sort(unique(ipc$area_text))
#======================================================#
### Pivoting wider for the variables
#======================================================#
# ipc <- ipc %>% pivot_wider(names_from = c(series_title), values_from = value, names_sep = "_")
#======================================================#
### Pivoting wider for the variables
#======================================================#
sort(unique(ipc$measure_text))
sort(unique(ipc$duration_text))
ipc.employment <- ipc %>%
  filter(measure_text == "Employment (Thousands of jobs)") %>%
  filter(duration_text == "Annual percent changes") %>%
  rename(employment = value)

ipc.workedhours <- ipc %>%
  filter(measure_text == "Hours worked (Millions of hours)") %>%
  filter(duration_text == "Annual percent changes") %>%
  rename(hourworked = value)

ipc.merge <- merge(ipc.employment, ipc.workedhours, by = c("area_text", "year")) %>%
  select(c(area_text, year, employment, hourworked)) %>%
  rename(state = area_text) %>%
  mutate(facility.state = ifelse(state == "California", "CA", "OR"))

#======================================================#
#### DOL - INDUSTRY PRODUCTIVITY COST -  bulk download
# #====================================================#
# ipc.area.filepath <- "C:/Users/Davidmac.Ekeocha/OneDrive/Documents/ULMS/PhD/Data_PhD/US/DOL/IPC/download.bls.gov_pub_time.series_ip_ip.area.txt"
# ipc.ind.filepath <- "C:/Users/Davidmac.Ekeocha/OneDrive/Documents/ULMS/PhD/Data_PhD/US/DOL/IPC/download.bls.gov_pub_time.series_ip_ip.industry.txt"
# ipc.data.filepath <- "C:/Users/Davidmac.Ekeocha/OneDrive/Documents/ULMS/PhD/Data_PhD/US/DOL/IPC/download.bls.gov_pub_time.series_ip_ip.data.1.AllData.txt"
# ipc.series.filepath <- "C:/Users/Davidmac.Ekeocha/OneDrive/Documents/ULMS/PhD/Data_PhD/US/DOL/IPC/download.bls.gov_pub_time.series_ip_ip.series.txt"
#
# ipc.area <- read.table(file = ipc.area.filepath, header = TRUE, sep = "\t")
# ipc.ind <- read.table(file = ipc.ind.filepath, header = TRUE, sep = "\t")
# ipc.data <- read.table(file = ipc.data.filepath, header = TRUE, sep = "\t")
# ipc.series <- read.table(file = ipc.series.filepath, header = TRUE, sep = "\t")
#
# # Print the DF
# head(ipc.series, n = 10)
# head(ipc.area, n = 10)
# head(ipc.ind, n = 10)
# head(ipc.data, n = 10)
#
# # Merging the data and removing national level data
# ipc <- merge(x = ipc.series, y = ipc.area)
# ipc <- merge(x = ipc, y = ipc.ind, by = "industry_code", all = T, no.dups = T, sort = T)
# ipc <- merge(x = ipc, y = ipc.data) %>%
#   select(!starts_with("footnote")) %>%
#   filter(area_code != 0)
# head(ipc, n = 10)
#
# # # Merging the data and removing national level data
# # ipc <- merge(x = ipc.series, y = ipc.area, by = "area_code", all = T, no.dups = T, sort = T)
# # ipc <- merge(x = ipc, y = ipc.ind, by = "industry_code", all = T, no.dups = T, sort = T)
# # ipc <- merge(x = ipc, y = ipc.data, by = "series_id", all = T, no.dups = T, sort = T) %>%
# #   select(!starts_with("footnote")) %>%
# #   filter(area_code != 0)
# # head(ipc, n = 10)
#
# ipc <- ipc %>%
#   select(c(area_code, area_text, series_id, series_title, year, measure_code,
#            type_code, duration_code, base_year, value)) %>%
#   filter(area_text != "District of Columbia") %>%
#   filter(area_text != "Midwest Region") %>%
#   filter(area_text != "Northeast Region") %>%
#   filter(area_text != "West Region") %>%
#   rename(state = area_text, state.code = area_code) %>%
#   group_by(state) %>%
#   mutate(state.code = row_number())
#
# ipc.al <- ipc %>% filter(state == "Alabama")
# ipc <- ipc %>% filter(state == "California" | state == "Oregon")
#
# ## Subsetting the data by series_id and pivot_wider
# # Annual hours worked (millions of hours) for private nonfarm
# ipc.L20 <- ipc %>%
#   filter(series_title == "Annual hours worked (millions of hours) for private nonfarm, California" |
#            series_title == "Annual hours worked (millions of hours) for private nonfarm, Oregon") %>%
#   # pivot_wider(names_from = "measure_code", values_from = "value") %>%
#   rename(annual.hours.worked.inmillionhrs = value) %>%
#   select(c(year, state, annual.hours.worked.inmillionhrs)) %>%
#   mutate(facility.state = ifelse(state == "California", yes = "CA", no = "OR"))
#
# # Annual real labor compensation (millions of 2012 dollars) for private nonfarm
# ipc.L06 <- ipc %>%
#   filter(measure_code == "L06")
#   # pivot_wider(names_from = "measure_code", values_from = "value") %>%
#   rename(annual.real.labour.compensation.inmilliondollars = L06) %>%
#   select(c(year, state, annual.real.labour.compensation.inmilliondollars)) %>%
#   mutate(facility.state = ifelse(state == "California", yes = "CA", no = "OR"))
#
# # Annual hourly compensation (current dollars per hour worked) for private nonfarm
# ipc.U13 <- ipc %>%
#   filter(measure_code == "U13") %>%
#   pivot_wider(names_from = "measure_code", values_from = "value") %>%
#   rename(annual.real.hourly.compensation.currentdollars = U13) %>%
#   select(c(year, state, annual.real.hourly.compensation.currentdollars))
#
# # Annual real value-added output (index, 2012=100) for private nonfarm
# ipc.T02 <- ipc %>%
#   filter(measure_code == "T02") %>%
#   pivot_wider(names_from = "measure_code", values_from = "value") %>%
#   rename(annual.real.value.added.index = T02) %>%
#   select(c(year, state, annual.real.value.added.index))
#
# # Annual output per employee (index, 2012=100) for private nonfarm
# ipc.W00 <- ipc %>%
#   filter(measure_code == "W00") %>%
#   pivot_wider(names_from = "measure_code", values_from = "value") %>%
#   rename(annual.output.per.employee.index = W00) %>%
#   select(c(year, state, annual.output.per.employee.index))
#
# # Annual unit labor costs (index, 2012=100) for private nonfarm
# ipc.U10 <- ipc %>%
#   filter(measure_code == "U10") %>%
#   pivot_wider(names_from = "measure_code", values_from = "value") %>%
#   rename(annual.unit.labour.cost.index = U10) %>%
#   select(c(year, state, annual.unit.labour.cost.index))
#
# # Annual labor productivity (index, 2012=100) for private nonfarm
# ipc.L00 <- ipc %>%
#   filter(measure_code == "L00") %>%
#   pivot_wider(names_from = "measure_code", values_from = "value") %>%
#   rename(annual.labour.productivity.index = L00) %>%
#   select(c(year, state, annual.labour.productivity.index))
#
# # Annual employment (thousands of jobs) for private nonfarm
# ipc.W20 <- ipc %>%
#   filter(measure_code == "W20") %>%
#   pivot_wider(names_from = c("measure_code"), values_from = "value") %>%
#   rename(annual.employment.inthousandsjobs = W20) %>%
#   select(c(year, state, annual.employment.inthousandsjobs))
#
# ipc1 <- cbind(ipc.L20, ipc.L06)
# ipc1 <- cbind(ipc.L20, ipc.L06, ipc.U13, ipc.T02, ipc.W00, ipc.U10, ipc.L00, ipc.W20)
#
#
# ipc.L20 <- ipc.L20[order(ipc.L20$year & ipc.L20$state, decreasing = F),]
# ipc1 <- left_join(x = ipc.L20, y = ipc.L06)
# ipc1 <- merge(x = ipc1, y = ipc.U13)
# ipc1 <- merge(x = ipc1, y = ipc.T02)
# ipc1 <- merge(x = ipc1, y = ipc.W00)
# ipc1 <- merge(x = ipc1, y = ipc.U10)
# ipc1 <- merge(x = ipc1, y = ipc.L00)
# ipc1 <- merge(x = ipc1, y = ipc.W20)
#
#
# # pivoting wider
# ipc <- ipc %>%
#   pivot_wider(
#     names_from = c("measure_code", "type_code", "series_id"),
#     values_from = "value"
#   )
#
# ipc <- ipc %>% pivot_longer(
#   cols = contains("L00"),
#   names_to = "L00",
#   values_to = "Annual labor productivity (index, 2012=100) for private nonfarm"
# )