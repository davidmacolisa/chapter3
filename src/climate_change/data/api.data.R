#======================================================#
### API AND DATA
#======================================================#
## Packages
#======================================================#
library(ndjson)
library(dplyr)
library(httr)
library(tidyr)
library(readr)
library(statar)
library(usgeogr)
#======================================================#
## Working Directory
#======================================================#
setwd("C:/Users/Davidmac.Ekeocha/OneDrive/Documents/ULMS/PhD/")
#======================================================#
### Centre for Disease Control and Protection PRAMStat Data for
#======================================================#
## Install the required package with:
## install.packages("RSocrata")
# install.packages(Rsocrata)
# library("RSocrata")

# apptoken <- "5abDwYIBArU5F7Wz7qP81w4uU"
# cdc.pram.2011 <- read.socrata(url = "https://data.cdc.gov/resource/ese6-rqpq.json", app_token = apptoken)
# cdc.pram.2009 <- read.socrata(url = "https://data.cdc.gov/resource/qwpv-wpc8.json", app_token = apptoken)


#======================================================#
#### Census - BUSINESS STATISTICS SURVEY - 2005 - 2021
#======================================================#
# All timeseries
# bds.timeseries.zip <- "Data_PhD/US/BDS/BDSTIMESERIES.zip"
# unzip(zipfile = bds.timeseries.zip, exdir =
# "C:/Users/david/OneDrive/Documents/ULMS/PhD/Data_PhD/US/BDS/BDSTIMESERIES")
# bds.timeseries.filepath <- "C:/Users/david/OneDrive/Documents/ULMS/PhD/Data_PhD/US/BDS/BDSTIMESERIES/BDSTIMESERIES
# .dat"
# bds.timeseries <- read.table(file = bds.timeseries.filepath, header = T, sep = "|")
# bds.timeseries <- bds.timeseries %>% filter(YEAR >= 1995)
# write_rds(bds.timeseries, file = "C:/Users/david/OneDrive/Documents/ULMS/PhD/Data_PhD/US/BDS/bds.timeries1995.rds",
# compress = "xz")
filepath <- "Data_PhD/US/BDS/bds.timeries1995.rds"
bds <- readRDS(file = filepath) %>% filter(YEAR >= 2005)

bdsm <- bds %>% filter(COUNTY != 0) #removing the national level
bdsm <- bdsm %>% filter(COUNTY != 999) #removing the statewide level
bdsm <- bdsm %>% filter(COUNTY != 886) #removing the offshore level
bdsm <- bdsm %>% filter(COUNTY != 998) #removing the unknown location level
bdsm <- bdsm %>% filter(NAICS_LABEL != "Total for all sectors") #removing national totals for all sectors

#======================================================================================================================#
filepath <- "Data_PhD/US/BDS/bds.timeries1995.rds"
bds <- readRDS(file = filepath) %>% filter(YEAR >= 2005)
bds_caor <- bds %>% subset(ST == 6 | ST == 41)
bds_caor <- bds_caor[grep(pattern = "^31", bds_caor$NAICS),] #subset for only manufacturing
sort(unique(bds_caor$NAICS))

bds_caor <- bds_caor %>% filter(COUNTY != 999) #removing the statewide level
bds_caor <- bds_caor %>% filter(COUNTY != 886) #removing the offshore level
bds_caor <- bds_caor %>% filter(COUNTY != 998) #removing the unknown location level
bds_caor <- bds_caor %>% subset(NAICS_LABEL != "Total for all sectors") #removing national totals for all sectors

filepath <- "Data_PhD/US/cbp21co.txt"
a <- fread(file = filepath, header = T, sep = ",")

#===============================================#
### select the 50 states and columns
#===============================================#
unique((bdsm$ST))
bdsm <- bdsm %>%
  filter(ST != 11) %>%
  mutate(
    state = case_when(
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
      ST == 33 ~ "Mew Hampshire",
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
      ST == 50 ~ "Vermont"
    )
  ) %>%
  select(, c(GEO_ID, YEAR, ST, state, COUNTY, NAME, NAICS, NAICS_LABEL, FIRM, ESTAB, EMP, DENOM, ESTABS_ENTRY,
             ESTABS_ENTRY_RATE, ESTABS_EXIT, ESTABS_EXIT_RATE, JOB_CREATION, JOB_CREATION_RATE,
             JOB_CREATION_BIRTHS, JOB_CREATION_RATE_BIRTHS, JOB_CREATION_CONTINUERS, JOB_DESTRUCTION,
             JOB_DESTRUCTION_RATE, JOB_DESTRUCTION_DEATHS, JOB_DESTRUCTION_RATE_DEATHS,
             JOB_DESTRUCTION_CONTINUERS, NET_JOB_CREATION, NET_JOB_CREATION_RATE, REALLOCATION_RATE,
             FIRMDEATH_FIRMS, FIRMDEATH_ESTABS, FIRMDEATH_EMP)) %>%
  rename(state.code = ST, county.code = COUNTY, county.name = NAME)

names(bdsm) <- tolower(names(bdsm)) # change column names to lowercase

manufacturing <- filter(bdsm, naics_label == "Manufacturing")
sort(unique(bdsm$year))

write_rds(x = bdsm, file = "Data_PhD/US/BDS/bdsm.rds", compress = "xz")
#======================================================================================================================#
# base_url <- ""
patent_url <- "https://developer.uspto.gov/api-catalog/bdss/"
api_key <- "knGsDaXndObaTjB5qrX9D2bBzczsEBSd"
full_url <- base::paste0(patent_url)

### API CALLS
patent <- httr::GET(full_url)
patent$status_code
patent$content


content_list <- httr::content(patent, as = "parsed")
parsed_content <- xml2::read_xml(content_list)

flattened_list <- jsonlite::flatten(content_list)
df <- as.data.frame(content_list)

### Convert to readable data
patent.char <- base::rawToChar(patent$content) # converts to
# characters
patent.json <- jsonlite::fromJSON(patent.char, flatten = TRUE)


filepath <- "Data_PhD/US/ad19800101-20221231-08.xml"
df <- xml2::read_xml(filepath)

df <- xml2::as_list(df) %>% purrr::flatten_dfc()
#======================================================================================================================#