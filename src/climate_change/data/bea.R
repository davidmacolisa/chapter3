#======================================================================================================================#
### BEA DATA ###
#======================================================================================================================#
## Packages
#======================================================================================================================#
library(ndjson)
library(dplyr)
library(httr)
library(tidyr)
library(readr)
library(statar)
library(usgeogr)
library(collapse) #collapse df
#======================================================================================================================#
## Working Directory
#======================================================================================================================#
setwd("C:/Users/Davidmac.Ekeocha/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
data(county_df, package = "usgeogr")
data(zip_df, package = "usgeogr")
data(cbcp_df, package = "usgeogr")
county_df <- data.frame(county_df)
zip_df <- data.frame(zip_df)
cbcp_df <- data.frame(cbcp_df)
#======================================================================================================================#
### Get parameters names
#======================================================================================================================#
base_url <- "https://apps.bea.gov/api/data/?&UserID="
api_key <- "A3796DAC-67A3-4BB5-A508-5C4513E467E7"
data <- "&method=GetParameterValues&datasetname=GDPbyIndustry&ParameterName=TableID&ResultFormat=json"
full_url <- base::paste0(base_url, api_key, data)

### API CALLS
bea <- httr::GET(full_url)
bea$status_code
bea$content

bea.char <- base::rawToChar(bea$content) # converts to characters
bea.json <- jsonlite::fromJSON(bea.char, flatten = TRUE)
# year <- bea.json$BEAAPI$Results$ParamValue
parameters <- bea.json$BEAAPI$Results$ParamValue
#======================================================================================================================#
### Gross Output by industry---at county level---not found
#======================================================================================================================#
base_url <- "https://apps.bea.gov/api/data/?&UserID="
api_key <- "A3796DAC-67A3-4BB5-A508-5C4513E467E7"
data <- "&method=GetData&datasetname=Regional&TableName=SQINC5N&LineCode=1&Year=2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022&GeoFips=COUNTY&ResultFormat=json"
full_url <- base::paste0(base_url, api_key, data)

### API CALLS
bea <- httr::GET(full_url)
bea$status_code
bea$content

bea.char <- base::rawToChar(bea$content) # converts to characters
bea.json <- jsonlite::fromJSON(bea.char, flatten = TRUE)
goutput.df <- bea.json$BEAAPI$Results$Data

# compensation_to_employees.df <- compensation_to_employees.df %>%
#   select(GeoFips, TimePeriod, CL_UNIT, DataValue) %>%
#   rename(fips_code = GeoFips) %>%
#   left_join(
#     y = county_df %>% select(fips_code, county_name, county_state),
#     by = c("fips_code" = "fips_code")
#   ) %>%
#   select(fips_code, county_name, county_state, TimePeriod, CL_UNIT, DataValue)
#======================================================================================================================#
### Compensation to employees by industry---at county level
#======================================================================================================================#
base_url <- "https://apps.bea.gov/api/data/?&UserID="
api_key <- "A3796DAC-67A3-4BB5-A508-5C4513E467E7"
data <- "&method=GetData&datasetname=Regional&TableName=CAINC6N&LineCode=1&Year=2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022&GeoFips=COUNTY&ResultFormat=json"
full_url <- base::paste0(base_url, api_key, data)

### API CALLS
bea <- httr::GET(full_url)
bea$status_code
bea$content

bea.char <- base::rawToChar(bea$content) # converts to characters
bea.json <- jsonlite::fromJSON(bea.char, flatten = TRUE)
compensation_to_employees.df <- bea.json$BEAAPI$Results$Data

compensation_to_employees.df <- compensation_to_employees.df %>%
  select(GeoFips, TimePeriod, CL_UNIT, DataValue) %>%
  rename(fips_code = GeoFips) %>%
  left_join(
    y = county_df %>% select(fips_code, county_name, county_state),
    by = c("fips_code" = "fips_code")
  ) %>%
  select(fips_code, county_name, county_state, TimePeriod, CL_UNIT, DataValue)
#======================================================================================================================#
### GDP by county and msa
#======================================================================================================================#
base_url <- "https://apps.bea.gov/api/data/?&UserID="
api_key <- "A3796DAC-67A3-4BB5-A508-5C4513E467E7"
data <- "&method=GetData&datasetname=Regional&TableName=CAGDP2&LineCode=1&Year=2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022&GeoFips=COUNTY&ResultFormat=json"
full_url <- base::paste0(base_url, api_key, data)

### API CALLS
bea <- httr::GET(full_url)
bea$status_code
bea$content

bea.char <- base::rawToChar(bea$content) # converts to characters
bea.json <- jsonlite::fromJSON(bea.char, flatten = TRUE)
gdp.df <- bea.json$BEAAPI$Results$Data

gdp.df <- gdp.df %>%
  select(GeoFips, TimePeriod, CL_UNIT, DataValue) %>%
  rename(fips_code = GeoFips) %>%
  left_join(
    y = county_df %>% select(fips_code, county_name, county_state),
    by = c("fips_code" = "fips_code")
  ) %>%
  select(fips_code, county_name, county_state, TimePeriod, CL_UNIT, DataValue) %>%
  lapply(., as.character)
#======================================================================================================================#
### RGDP by county and msa
#======================================================================================================================#
base_url <- "https://apps.bea.gov/api/data/?&UserID="
api_key <- "A3796DAC-67A3-4BB5-A508-5C4513E467E7"
data <- "&method=GetData&datasetname=Regional&TableName=CAGDP9&LineCode=1&Year=2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022&GeoFips=COUNTY&ResultFormat=json"
full_url <- base::paste0(base_url, api_key, data)

### API CALLS
bea <- httr::GET(full_url)
bea$status_code
bea$content

bea.char <- base::rawToChar(bea$content) # converts to characters
bea.json <- jsonlite::fromJSON(bea.char, flatten = TRUE)
rgdp.df <- bea.json$BEAAPI$Results$Data

rgdp.df <- rgdp.df %>%
  select(GeoFips, TimePeriod, CL_UNIT, DataValue) %>%
  rename(fips_code = GeoFips) %>%
  left_join(
    y = county_df %>% select(fips_code, county_name, county_state),
    by = c("fips_code" = "fips_code")
  ) %>%
  select(fips_code, county_name, county_state, TimePeriod, CL_UNIT, DataValue) %>%
  lapply(., as.character)
#======================================================================================================================#
### Personal Income---county-level
#======================================================================================================================#
base_url <- "https://apps.bea.gov/api/data/?&UserID="
api_key <- "A3796DAC-67A3-4BB5-A508-5C4513E467E7"
data <- "&method=GetData&datasetname=Regional&TableName=CAINC1&LineCode=1&Year=2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022&GeoFips=COUNTY&ResultFormat=json"
full_url <- base::paste0(base_url, api_key, data)

### API CALLS
bea <- httr::GET(full_url)
bea$status_code
bea$content

bea.char <- base::rawToChar(bea$content) # converts to characters
bea.json <- jsonlite::fromJSON(bea.char, flatten = TRUE)
pincome.df <- bea.json$BEAAPI$Results$Data

pincome.df <- pincome.df %>%
  select(GeoFips, TimePeriod, CL_UNIT, DataValue) %>%
  rename(fips_code = GeoFips) %>%
  left_join(
    y = county_df %>% select(fips_code, county_name, county_state),
    by = c("fips_code" = "fips_code")
  ) %>%
  select(fips_code, county_name, county_state, TimePeriod, CL_UNIT, DataValue)

### Replacing missing values in county gdp with personal income from 2009-2016.
pincome.aug.df <- filter(pincome.df, TimePeriod <= 2016) %>% lapply(., as.character)
gdp.new.df <- bind_rows(gdp.df, pincome.aug.df)
gdp.new.df <- gdp.new.df[!duplicated(gdp.new.df),]
gdp.new.df <- gdp.new.df[order(gdp.new.df$TimePeriod),]
#======================================================================================================================#
### Regional Price Parities by State
#======================================================================================================================#
base_url <- "https://apps.bea.gov/api/data/?&UserID="
api_key <- "A3796DAC-67A3-4BB5-A508-5C4513E467E7"
data <- "&method=GetData&datasetname=Regional&TableName=SARPP&LineCode=1&Year=2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022&GeoFips=STATE&ResultFormat=json"
full_url <- base::paste0(base_url, api_key, data)

### API CALLS
bea <- httr::GET(full_url)
bea$status_code
bea$content
bea.char <- base::rawToChar(bea$content) # converts to characters
bea.json <- jsonlite::fromJSON(bea.char, flatten = TRUE)
regional_price_parity.df <- bea.json$BEAAPI$Results$Data

regional_price_parity.df <- regional_price_parity.df %>%
  select(GeoFips, GeoName, TimePeriod, CL_UNIT, DataValue) %>%
  filter(!GeoName %in% c("District of Columbia", "Alaska", "Hawaii", "United States")) %>%
  mutate(
    county_state = case_when(
      GeoName == "Alabama" ~ "AL",
      GeoName == "Arizona" ~ "AR",
      GeoName == "Arkansas" ~ "AZ",
      GeoName == "California" ~ "CA",
      GeoName == "Colorado" ~ "CO",
      GeoName == "Connecticut" ~ "CT",
      GeoName == "Delaware" ~ "DE",
      GeoName == "Florida" ~ "FL",
      GeoName == "Georgia" ~ "GA",
      GeoName == "Iowa" ~ "IA",
      GeoName == "Idaho" ~ "ID",
      GeoName == "Illinois" ~ "IL",
      GeoName == "Indiana" ~ "IN",
      GeoName == "Kansas" ~ "KS",
      GeoName == "Kentucky" ~ "KY",
      GeoName == "Louisiana" ~ "LA",
      GeoName == "Massachusetts" ~ "MA",
      GeoName == "Maryland" ~ "MD",
      GeoName == "Maine" ~ "ME",
      GeoName == "Michigan" ~ "MI",
      GeoName == "Minnesota" ~ "MN",
      GeoName == "Missouri" ~ "MO",
      GeoName == "Mississippi" ~ "MS",
      GeoName == "Montana" ~ "MT",
      GeoName == "North Carolina" ~ "NC",
      GeoName == "North Dakota" ~ "ND",
      GeoName == "Nebraska" ~ "NE",
      GeoName == "New Hampshire" ~ "NH",
      GeoName == "New Jersey" ~ "NJ",
      GeoName == "New Mexico" ~ "NM",
      GeoName == "Nevada" ~ "NV",
      GeoName == "New York" ~ "NY",
      GeoName == "Ohio" ~ "OH",
      GeoName == "Oklahoma" ~ "OK",
      GeoName == "Oregon" ~ "OR",
      GeoName == "Pennsylvania" ~ "PA",
      GeoName == "Rhode Island" ~ "RI",
      GeoName == "South Carolina" ~ "SC",
      GeoName == "South Dakota" ~ "SD",
      GeoName == "Tennessee" ~ "TN",
      GeoName == "Texas" ~ "TX",
      GeoName == "Utah" ~ "UT",
      GeoName == "Virginia" ~ "VA",
      GeoName == "Vermont" ~ "VT",
      GeoName == "Washington" ~ "WA",
      GeoName == "Wisconsin" ~ "WI",
      GeoName == "West Virginia" ~ "WV",
      GeoName == "Wyoming" ~ "WY",
    )
  ) %>%
  select(-GeoName) %>%
  rename(state_code = GeoFips) %>%
  left_join(
    y = county_df %>% select(county_state),
    by = c("county_state" = "county_state")
  ) %>%
  select(state_code, county_state, TimePeriod, CL_UNIT, DataValue)

regional_price_parity.df <- regional_price_parity.df %>%
  collap(
    .,
    ~state_code + TimePeriod,
    na.rm = T,
    FUN = fmean,
    keep.col.order = T,
    sort = T,
    decreasing = F,
  )

bea <- pincome.df %>%
  rename(personal_income = DataValue) %>%
  left_join(
    y = gdp.new.df %>% rename(gdp = DataValue),
    by = c("fips_code" = "fips_code", "county_name" = "county_name", "county_state" = "county_state",
           "TimePeriod" = "TimePeriod", "CL_UNIT" = "CL_UNIT")
  ) %>%
  left_join(
    y = compensation_to_employees.df %>% rename(compensation_to_employees = DataValue),
    by = c("fips_code" = "fips_code", "county_name" = "county_name", "county_state" = "county_state",
           "TimePeriod" = "TimePeriod", "CL_UNIT" = "CL_UNIT")
  ) %>%
  left_join(
    y = regional_price_parity.df %>% rename(regional_price_parity = DataValue, bea_rpp_unit = CL_UNIT),
    by = c("county_state" = "county_state", "TimePeriod" = "TimePeriod")
  ) %>%
  rename(bea_unit = CL_UNIT, year = TimePeriod) %>%
  select(
    c(
      fips_code, county_name, county_state, personal_income, gdp, compensation_to_employees,
      bea_unit, regional_price_parity, bea_rpp_unit, year
    )
  ) %>%
  data.frame() %>%
  write_rds(., file = "./Data_PhD/US/BEA/bea.rds", compress = "xz")
#======================================================================================================================#