#======================================================================================================================#
### Packages
#======================================================================================================================#
gc()
library(tidyverse)
library(stringr)
library(statar)
library(usgeogr)
#======================================================================================================================#
### Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading shapefiles---State and County borders
#======================================================================================================================#
source(file = "./Thesis/chapter3/src/climate_change/data/border_state_county_df.R", echo = T)
#======================================================================================================================#
### Loading US county data
#======================================================================================================================#
data(county_df, package = "usgeogr")
county_df <- county_df %>%
  select(fips_code, county_name, county_state) %>%
  data.frame()
county_df$county_name <- gsub(pattern = "\\b(\\w+)$", replacement = "", x = county_df$county_name)
#======================================================================================================================#
### Loading Data: TRI---Form R---and merging GHGP from EPA
gc()
#======================================================================================================================#
filepath <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/triR.rds"
start_time <- Sys.time()
tri <- readRDS(file = filepath) %>%
  filter(year >= 2011 & year <= 2017) %>%
  select(., -c(facility.state.code, naics, naics.sector.code)) %>%
  filter(!facility.state %in% c("AK", "HI")) %>%
  data.frame()

# Making first letters uppercase
tri$facility.county <- tolower(tri$facility.county)
tri$facility.county <- stringi::stri_trans_totitle(tri$facility.county)

tri$facility.city <- tolower(tri$facility.city)
tri$facility.city <- stringi::stri_trans_totitle(tri$facility.city)

tri$offsite.city <- iconv(tri$offsite.city, to = "UTF-8")
tri$offsite.city <- tolower(tri$offsite.city)
tri$offsite.city <- stringi::stri_trans_totitle(tri$offsite.city)

tri$offsite.county <- tolower(tri$offsite.county)
tri$offsite.county <- stringi::stri_trans_totitle(tri$offsite.county)

tri$offsite.province <- iconv(tri$offsite.province, to = "UTF-8")
tri$offsite.province <- tolower(tri$offsite.province)
tri$offsite.province <- stringi::stri_trans_totitle(tri$offsite.province)

tri$potw.city <- iconv(tri$potw.city, to = "UTF-8")
tri$potw.city <- tolower(tri$potw.city)
tri$potw.city <- stringi::stri_trans_totitle(tri$potw.city)

tri$potw.county <- tolower(tri$potw.county)
tri$potw.county <- stringi::stri_trans_totitle(tri$potw.county)
end_time <- Sys.time()
end_time - start_time
gc()
#======================================================================================================================#
### Merge zip_df and county_df with tri data to get the fips_code
#======================================================================================================================#
gc()
start_time <- Sys.time()
triM <- tri %>%
  filter(year >= 2011 & year <= 2017) %>%
  filter(
    facility.state %in% c(
      #treated states
      "AR", "CA", "DE", "ME", "MA", "MD", "MI", "MN", "NE", "NY", "WV",
      #control states
      "GA", "IA", "ID", "IL", "IN", "KS", "KY", "NH", "NM", "NV", "NC",
      "ND", "OK", "PA", "TX", "UT", "VA", "WI", "WY"
    )
  ) %>%
  group_by(facility.state, facility.county) %>%
  mutate(
    facility.longitude = as.numeric(facility.longitude),
    facility.latitude = as.numeric(facility.latitude)
  ) %>%
  left_join(
    y = zip_df %>% select(c(fips_code, zip_code)),
    by = c("facility.zipcode" = "zip_code")
  ) %>%
  left_join(
    y = county_df,
    by = c("facility.state" = "county_state", "facility.county" = "county_name")
  ) %>%
  # mutate(fips_code = ifelse(is.na(fips_code.x), fips_code.y, fips_code.x)) %>%
  # select(-c(fips_code.x, fips_code.y))  # Remove extra columns
data.frame()
end_time <- Sys.time()
end_time - start_time
gc()
#======================================================================================================================#
### Fixing fips codes in triM
#======================================================================================================================
## 1. Create a temporary data frame with full information for matching:
library(dplyr)

# Assuming 'df1' and 'df2' are your dataframes

# Fill NAs in df1$fips_code with corresponding values from df2$fips_code based on matching county_state and county_name
triM <- triM %>%
  mutate(fips_code = coalesce(fips_code, county_df$fips_code[match(paste(triM$facility.state, triM$facility.name),
                                                                   paste(county_df$county_state, county_df$county_name))]))

# Now df1$fips_code will contain the NAs filled with corresponding values from df2$fips_code based on matching county_state and county_name.

n_distinct(triM$fips_code)

triM$fips_code[triM$facility.state == "MA" & triM$facility.county == "Berkshire"] <- 25003
triM$fips_code[triM$facility.state == "MA" & triM$facility.county == "Essex"] <- 25009
triM$fips_code[triM$facility.state == "MA" & triM$facility.county == "Franklin"] <- 25011
triM$fips_code[triM$facility.state == "MA" & triM$facility.county == "Hampshire"] <- 25015
triM$fips_code[triM$facility.state == "MA" & triM$facility.county == "Hampden"] <- 25013
triM$fips_code[triM$facility.state == "MA" & triM$facility.county == "Middlesex"] <- 25017
triM$fips_code[triM$facility.state == "MA" & triM$facility.county == "Norfolk"] <- 25021
triM$fips_code[triM$facility.state == "MA" & triM$facility.county == "Suffolk"] <- 25025
triM$fips_code[triM$facility.state == "MA" & triM$facility.county == "Worcester"] <- 25027


triM$fips_code[triM$facility.state == "MA" & triM$facility.county == "Worcester"] <- 25027
triM$fips_code[triM$facility.state == "MA" & triM$facility.county == "Worcester"] <- 25027
triM$fips_code[triM$facility.state == "MA" & triM$facility.county == "Worcester"] <- 25027
n_distinct(triM$fips_code)
n_distinct(triM$facility.state)
n_distinct(triM$facility.id)
n_distinct(triM$facility.county)
n_distinct(triM$facility.zipcode)
n_distinct(triM$naics.code)
n_distinct(triM$industry.name)

sort(unique(triM$facility.state))
sort(unique(triM$facility.id))
sort(unique(triM$fips_code))
sort(unique(triM$relaxed_cpcp_id))
sort(unique(triM$state))
sort(unique(triM$naics.code))
sort(unique(triM$industry.name))
sort(unique(triM$chemical.name))
gc()
#======================================================================================================================#
### Keeping only common facility states across years---Panelize the facility.state
#======================================================================================================================#
gc()
# Split the facility id column into a list of vectors by year
facility.state_by_year <- split(triM$facility.state, triM$year)

# Find the common chemicals across all states
common_facility.state <- Reduce(f = intersect, x = facility.state_by_year)

# Output the common chemicals
print(common_facility.state)

# Keep only common facility ids across years in the dataframe
triM <- triM %>% filter(facility.state %in% common_facility.state)
sort(unique(triM$state))
gc()
#======================================================================================================================#
### Keeping only common facility.id across years---Panelize the facility.ids
#======================================================================================================================#
# Split the facility id column into a list of vectors by year
facility.id_by_year <- split(triM$facility.id, triM$year)

# Find the common chemicals across all states
common_facility.id <- Reduce(f = intersect, x = facility.id_by_year)

# Output the common chemicals
print(common_facility.id)

# Keep only common facility ids across years in the dataframe
triM <- triM %>% filter(!facility.id %in% common_facility.id)
#======================================================================================================================#
### Keeping only the common chemicals between the treated and the control states---Panelize the chemicals
#======================================================================================================================#
gc()
# Split the chemicals column into a list of vectors by state
chemicals_by_state <- split(triM$chemical.name, triM$facility.state)

# Find the common chemicals across all states
common_chemicals <- Reduce(f = intersect, x = chemicals_by_state)

# Output the common chemicals
print(common_chemicals)

# Keep common chemicals in the dataframe
triM <- triM %>% filter(chemical.name %in% common_chemicals)
sort(unique(triM$state))
gc()
#======================================================================================================================#
### Merge ghgp with tri
#======================================================================================================================#
# ghgp <- read_rds(file = "./Data_PhD/US/EPA/AQS/ghg/ghgp.rds") %>% data.frame()
#
# # Remove last word in county labels
# ghgp$facility.county <- sub(pattern = "\\s+\\w+$", replacement = "", ghgp$facility.county)
#
# start_time <- Sys.time()
# triM <- triM %>%
#   left_join(
#     y = ghgp %>%
#       select(-c(facility.id, facility.state.code, facility.city, facility.zipcode )) %>%
#       rename(facility.id = facility.frs.id) %>%
#       mutate(year = as.numeric(year), ghgp.unit = "tons"),
#     by = c("year" = "year", "facility.county" = "facility.county",
#            "naics.code" = "naics.code", "facility.id" = "facility.id")
#   )
# end_time <- Sys.time()
# end_time - start_time

#======================================================================================================================#
### SEC Data
#======================================================================================================================#
# sec <- read_rds(file = "./Data_PhD/US/SEC/sec.rds") %>%
#   filter(!stprba %in% c("AK", "HI", "DC", ""))
# # sec.BS <- filter(sec, stmt == "BS") #firm balance sheet data
#
# # Making first letters uppercase
# sec$name <- tolower(sec$name)  # Convert entire column to lowercase
# sec$name <- gsub(pattern = "(^|\\s)([a-z])", replacement = "\\1\\U\\2", sec$name, perl = TRUE)  # Capitalize first
# letter of each word
#
# sec$cityba <- tolower(sec$cityba)
# sec$cityba <- gsub(pattern = "(^|\\s)([a-z])", replacement = "\\1\\U\\2", sec$cityba, perl = TRUE)  # Capitalize
# first letter of each word
# #======================================================================================================================#
# ### Assets
# #======================================================================================================================#
# sec.Assets <- sec %>%
#   filter(
#     tlabel %in% c("Assets", "Total Assets") &
#       datatype %in% "monetary" &
#       grepl(pattern = "Q", x = fp)
#   ) %>%
#   rename(assets = value, assets_label = tlabel, assets_doc = doc) %>%
#   mutate(assets = as.numeric(assets)) %>%
#   select(adsh, name, stprba, cityba, zipba, sic, year, assets) %>%
#   data.frame()
#
# sort(unique(sec.Assets$stprba))
# sort(unique(sec.Assets$year))
# n_distinct(sec.Assets$name)
#
# library(collapse) # for collapsing dataframe
# sec.Assets <- collap(
#   sec.Assets,
#   ~name + year,
#   na.rm = T,
#   FUN = fmean,
#   keep.col.order = T,
#   sort = T,
#   decreasing = F,
# )
#
# sort(unique(sec.Assets$stprba))
# sort(unique(sec.Assets$name))
# sec.Assets <- sec.Assets[complete.cases(sec.Assets),]
#
# sec.Assets <- sec.Assets %>%
#   rename(zip_code = zipba, facility.name = name) %>%
#   select(c(facility.name, zip_code, sic, assets, year)) %>%
#   left_join(
#     y = zip_df %>% select(c(fips_code, zip_code)),
#     by = ("zip_code" = "zip_code")
#   ) %>%
#   data.frame()
#======================================================================================================================#
### Loading QCEW---County Levels
#======================================================================================================================#
gc()
gc()
start_time <- Sys.time()
qcew <- read_rds(file = "./Data_PhD/US/BLS/qcew.rds") %>%
  filter(year >= 2011 & year <= 2017) %>%
  filter(own_code %in% c("1", "2", "3", "4", "5")) %>% # Ownership codes
  filter(agglvl_code %in% "78") %>% #county level 6-digits naics code, aggreagation level codes
  select(area_fips:avg_annual_pay, oty_total_annual_wages_chg, oty_total_annual_wages_pct_chg,
         oty_avg_annual_pay_chg, oty_avg_annual_pay_pct_chg) %>%
  rename(naics.code = industry_code, fips_code = area_fips)
end_time <- Sys.time()
end_time - start_time
gc()
gc()
#======================================================================================================================#
### Merging TRI, BEA and QCEW Data
#======================================================================================================================#
bea <- read_rds(file = "./Data_PhD/US/BEA/bea.rds") %>% filter(year >= 2011 & year <= 2017)
# Remove last word in county labels
bea$county_name <- sub(pattern = "\\s+\\w+$", replacement = "", bea$county_name)

start_time <- Sys.time()
triQ <- triM %>%
  group_by(fips_code, naics.code, year) %>%
  mutate(
    year = as.numeric(year),
  ) %>%
  # Join BEA data for county level macro data
  left_join(
    y = bea %>%
      # select(-c(county_name)) %>%
      rename(facility.state = county_state, facility.county = county_name) %>%
      mutate(year = as.numeric(year)),
    by = c("fips_code" = "fips_code", "facility.county" = "facility.county", "facility.state" = "facility.state",
           "year" = "year"),
  ) %>%
  left_join(
    y = qcew %>%
      mutate(year = as.numeric(year)),
    by = c("naics.code" = "naics.code", "fips_code" = "fips_code", "year" = "year"),
  ) %>%
  select(-c(qtr, disclosure_code)) %>%
  data.frame()
end_time <- Sys.time()
end_time - start_time
gc()

sort(unique(triQ$state))
sort(unique(triM$facility.state))
sort(unique(triM$state_border_id))
sort(unique(triM$nearest_border))

#======================================================================================================================#
### Merging NBER-CES DATA---Manufacturing Industry Database
### Based NAICS 2012
#======================================================================================================================#
gc()
start_time <- Sys.time()
triQ.manu <- triQ %>%
  filter(industry.category == "Manufacturing") %>%
  right_join(
    # y = read_csv(file = "./Data_PhD/US/NBER/nberces5818v1_n1997.csv") %>%
    y = read_csv(file = "./Data_PhD/US/NBER/nberces5818v1_n2012.csv") %>%
      filter(year >= 2011 & year <= 2017) %>%
      mutate(naics.code = as.character(naics)) %>%
      select(c(naics.code, year:plant, dtfp5)) %>%
      data.frame(),
    by = c("year" = "year", "naics.code" = "naics.code")
  ) %>%
  rename(facility.state.code = facility.state, facility.state = state) %>%
  data.frame()
end_time <- Sys.time()
end_time - start_time
gc()

n_distinct(triQ.manu$facility.state)
sort(unique(triQ.manu$facility.state))

gc()
start_time <- Sys.time()
write_rds(x = triQ.manu, file = "./Data_PhD/US/BLS/triQ.manu.rds", compress = "xz")
end_time <- Sys.time()
end_time - start_time
gc()
sum_up(triQ.manu, c(total.fug.air.emissions.onsite, total.point.air.emissions.onsite, total.air.emissions.onsite))
gc()

#======================================================================================================================#
### Merging triM to fac_county_df or fac_states_df for county level and state level analysis.
#======================================================================================================================#
triM <- triM[complete.cases(triM$treated.match),]

n_distinct(triM$fips_code)
n_distinct(triM$facility.id)
n_distinct(triM$state.code)
sort(unique(triM$state))

n_distinct(triM$facility.state)
triM <- triM[complete.cases(triM$treated.match),]

n_distinct(triM$fips_code)
n_distinct(triM$facility.id)
n_distinct(triM$state.code)
sort(unique(triM$state))

n_distinct(triM$facility.state)
#======================================================================================================================#