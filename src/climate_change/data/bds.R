#======================================================#
### API AND DATA
#======================================================#
## Packages
#======================================================#
# library(ndjson)
library(dplyr)
# library(httr)
library(tidyr)
library(readr)
library(statar)
library(usgeogr)
#======================================================#
## Working Directory
#======================================================#
setwd("C:/Users/Davidmac.Ekeocha/OneDrive/Documents/ULMS/PhD/")
#======================================================#
#### Census - BUSINESS STATISTICS SURVEY - 2005 - 2021
#======================================================#
# All timeseries
# bds.timeseries.zip <- "C:/Users/david/OneDrive/Documents/ULMS/PhD/Data_PhD/US/BDS/BDSTIMESERIES.zip"
# unzip(zipfile = bds.timeseries.zip, exdir =
# "C:/Users/david/OneDrive/Documents/ULMS/PhD/Data_PhD/US/BDS/BDSTIMESERIES")
# bds.timeseries.filepath <- "C:/Users/david/OneDrive/Documents/ULMS/PhD/Data_PhD/US/BDS/BDSTIMESERIES/BDSTIMESERIES
# .dat"
# bds.timeseries <- read.table(file = bds.timeseries.filepath, header = T, sep = "|")
# bds.timeseries <- bds.timeseries %>% filter(YEAR >= 1995)
# write_rds(bds.timeseries, file = "C:/Users/david/OneDrive/Documents/ULMS/PhD/Data_PhD/US/BDS/bds.timeries1995.rds",
# compress = "xz")
filepath <- "C:/Users/Davidmac.Ekeocha/OneDrive/Documents/ULMS/PhD/Data_PhD/US/BDS/bds.timeries1995.rds"
bds <- readRDS(file = filepath) %>% filter(YEAR >= 2009)
sort(unique(bds$INDLEVEL))

bdsm <- bds %>% filter(INDLEVEL == 2) #getting naics sectors
bdsm <- bdsm %>% filter(COUNTY != 0) #removing the national level
bdsm <- bdsm %>% filter(COUNTY != 999) #removing the statewide level
bdsm <- bdsm %>% filter(COUNTY != 886) #removing the offshore level
bdsm <- bdsm %>% filter(COUNTY != 998) #removing the unknown location level
bdsm <- bdsm %>% filter(NAICS_LABEL != "Total for all sectors") #removing national totals for all sectors

sort(unique(bdsm$ST))
bdsm <- bdsm %>%
  filter(ST != 11) %>%
  mutate(
    county_state = case_when(
      ST == 1 ~ "Alabama",
      ST == 2 ~ "Alaska",
      ST == 4 ~ "Arizona",
      ST == 5 ~ "Arkansas",
      ST == 6 ~ "California",
      ST == 8 ~ "Colorado",
      ST == 9 ~ "Connecticut",
      ST == 10 ~ "Delaware",
      ST == 12 ~ "Florida",
      ST == 13 ~ "Georgia",
      ST == 15 ~ "Hawaii",
      ST == 16 ~ "Idaho",
      ST == 17 ~ "Illinois",
      ST == 18 ~ "Indiana",
      ST == 19 ~ "Iowa",
      ST == 20 ~ "Kansas",
      ST == 21 ~ "Kentucky",
      ST == 22 ~ "Louisiana",
      ST == 23 ~ "Maine",
      ST == 24 ~ "Maryland",
      ST == 25 ~ "Massachussetts",
      ST == 26 ~ "Michigan",
      ST == 27 ~ "Minnesota",
      ST == 28 ~ "Mississippi",
      ST == 29 ~ "Missouri",
      ST == 30 ~ "Montana",
      ST == 31 ~ "Nebraska",
      ST == 32 ~ "Nevada",
      ST == 33 ~ "New Hampshire",
      ST == 34 ~ "New Jersey",
      ST == 35 ~ "New Mexico",
      ST == 36 ~ "New York",
      ST == 37 ~ "North Carolina",
      ST == 38 ~ "North Dakota",
      ST == 39 ~ "Ohio",
      ST == 40 ~ "Oklahoma",
      ST == 41 ~ "Oregon",
      ST == 42 ~ "Pennsylvania",
      ST == 44 ~ "Rhode Island",
      ST == 45 ~ "South Carolina",
      ST == 46 ~ "South Dakota",
      ST == 47 ~ "Tennessee",
      ST == 48 ~ "Texas",
      ST == 49 ~ "Utah",
      ST == 50 ~ "Vermont",
      ST == 51 ~ "Virginia",
      ST == 53 ~ "Washington",
      ST == 54 ~ "West Virginia",
      ST == 55 ~ "Wisconsin",
      ST == 56 ~ "Wyoming",
    )
  )

names(bdsm) <- tolower(names(bdsm)) # change column names to lowercase
bdsm <- bdsm %>%
  select(
    c(year, st, county_state, county, geo_id, name, naics, naics_label, sector, firm, estab, emp, denom, estabs_entry,
      estabs_entry_rate, estabs_exit, estabs_exit_rate, job_creation, job_creation_rate, job_creation_births,
      job_creation_rate_births, job_creation_continuers, job_destruction, job_destruction_rate, job_destruction_deaths,
      job_destruction_rate_deaths, job_destruction_continuers, net_job_creation, net_job_creation_rate, reallocation_rate,
      firmdeath_firms, firmdeath_estabs, firmdeath_emp)
  )

# removing the state names attahced to the county names
bdsm$name <- gsub(pattern = "\\b(\\w+)$", replacement = "", x = bdsm$name)
sort(unique(bdsm$name))
#===============================================#
### select the 50 states and columns
#===============================================#
#getting zip and county codes and names from the usgeogr file
data(zip_df)
data(county_df)
data(cbcp_df)
county_df <- as.data.frame(county_df)
zip_df <- as.data.frame(zip_df)
cbcp_df <- as.data.frame(cbcp_df)

bdsm <- bdsm %>%
  rename(fips_code = geo_id, county_name = name) %>%
  mutate(
    county_name = as.character(county_name),
  ) %>%
  left_join(
    y = county_df,
    by = c("county_state" = "county_state", "fips_code" = "fips_code"),
  )


write_rds(x = bdsm, file = "./Data_PhD/US/BDS/bdsm.rds", compress = "xz")

filepath <- "./Data_PhD/US/BDS/bdsm.rds"
bds <- readRDS(file = filepath)
bdsm <- bds %>%
  filter(state == "California" | state == "Oregon") %>%
  rename(facility.county.name = county.name) %>%
  mutate(facility.state = ifelse(state == "California", "CA", "OR"))


bdsm.tri <- merge(x = bdsm, y = tri, by = c("year"))
bdsm.tri.ipc <- merge(x = bdsm.tri, y = ipc.merge)
unique(bdsm.tri.ipc$naics_label)
unique(bdsm.tri$naics_label)
unique(tri.bdsm$state)
unique(tri.bdsm$facility.state)
sum_up()