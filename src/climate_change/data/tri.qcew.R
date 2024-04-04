gc()
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
### getting zip and county codes and names from the usgeogr file
### County-border pairs and assign each border county to a unique state border pair strip.
#======================================================================================================================#
data(zip_df) %>% data.frame()
data(county_df) %>% data.frame()
data(cbcp_df) %>% data.frame()
data(cbcounty_df) %>% data.frame()
data(sbscp_df) %>% data.frame()
data(state_df) %>% data.frame()

county_data <- county_df %>%
  select(-c(lat, long)) %>%
  left_join(
	y = state_df %>% rename(county_state = state_code),
	by = c("county_state" = "county_state")
  ) %>%
  # Get the neighbouring states
  # left_join(
  # y = )
  # Joining zip_codes
  right_join(
	y = zip_df %>%
	  select(c(fips_code, zip_code, state)) %>%
	  rename(county_state = state),
	by = c("fips_code" = "fips_code", "county_state" = "county_state")
  ) %>%
  # Assigns each within border county to a distinct "cross-border cluster"
  right_join(
	y = cbcounty_df %>%
	  rename(county_dist_to_border = dist_to_border, county_dist_to_segment = dist_to_segment) %>%
	  select(fips_code, county_state, state_border_id, relaxed_cpcp_id, cpcp_id, county_dist_to_border,
			 county_dist_to_segment),
	by = c("fips_code" = "fips_code", "county_state" = "county_state")
  ) %>%
  # Assigns each border county to a unique state border pair strip
  right_join(
	y = sbscp_df %>% select(fips_code, county_state, num_counties_in_strip, num_states_in_strip),
	by = c("fips_code" = "fips_code", "county_state" = "county_state")
  ) %>%
  select(
	c(zip_code, fips_code, county_name, county_state, state, relaxed_cpcp_id, state_border_id,
	  county_dist_to_border, county_dist_to_segment, num_counties_in_strip, num_states_in_strip, population)
  ) %>%
  # remove DC as it is not a state.
  filter(!county_state %in% "DC") %>%
  data.frame()

# Remove last word in county labels
county_data$county_name <- sub(pattern = "\\s+\\w+$", replacement = "", county_data$county_name)

# Making first letters uppercase
county_data$state <- tolower(county_data$state)  # Convert entire column to lowercase
county_data$state <- gsub(pattern = "(^|\\s)([a-z])", replacement = "\\1\\U\\2", county_data$state, perl = TRUE)  #
# Capitalize first letter of each word

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
### Merge county_data with tri data
#======================================================================================================================#
# merging zip_df with tri data to get the fips codes.
start_time <- Sys.time()
triM <- tri %>%
  group_by(naics.code, facility.zipcode, facility.county) %>%
  mutate(
	facility.longitude = as.numeric(facility.longitude),
	facility.latitude = as.numeric(facility.latitude)
  ) %>%
  left_join(
	y = county_data %>%
	  select(-zip_code) %>%
	  rename(facility.state = county_state, facility.county = county_name),
	by = c("fips_code" = "fips_code", "facility.county" = "facility.county", "facility.state" = "facility.state")
  ) %>%
  data.frame()
end_time <- Sys.time()
end_time - start_time

gc()
start_time <- Sys.time()
triM <- triM[complete.cases(triM$facility.id),]
triM <- triM[complete.cases(triM$facility.county),]
triM <- triM[complete.cases(triM$fips_code),]
triM <- triM[complete.cases(triM$relaxed_cpcp_id),]
end_time <- Sys.time()
end_time - start_time

sort(unique(triM$facility.id))
sort(unique(triM$fips_code))
sort(unique(triM$relaxed_cpcp_id))
sort(unique(triM$facility.state))
sort(unique(triM$naics.code))
sort(unique(triM$industry.name))
sort(unique(triM$chemical.name))

n_distinct(triM$facility.id)
n_distinct(triM$facility.county)
n_distinct(triM$facility.zipcode)
n_distinct(triM$fips_code)
n_distinct(triM$facility.state)
n_distinct(triM$naics.code)
n_distinct(triM$industry.name)

#======================================================================================================================#
### Getting the treated and controls states
#======================================================================================================================#
sort(unique(triM$state))
sort(unique(triM$facility.state))
sort(unique(triM$state_border_id))

# Selecting the treated and control states
start_time <- Sys.time()
triM <- triM %>%
  filter(year >= 2011 & year <= 2017) %>%
  filter(
	state %in% c( #treated states
	  "Arkansas", "California", "Delaware", "Maryland", "Michigan", "Minnesota",
	  "Nebraska", "New York", "West Virginia",
	  #control states
	  "Georgia", "Iowa", "Idaho", "Illinois", "Indiana", "Kansas", "Kentucky", "New Mexico",
	  "Nevada", "North Carolina", "North Dakota", "Oklahoma", "Pennsylvania", "Texas", "Utah", "Virginia",
	  "Wisconsin", "Wyoming"
	)
  )
end_time <- Sys.time()
end_time - start_time
sort(unique(triM$state))
#======================================================================================================================#
### Subsetting common facility states across years---Panelize the facility.state
#======================================================================================================================#
# Split the facility id column into a list of vectors by year
facility.state_by_year <- split(triM$facility.state, triM$year)

# Find the common chemicals across all states
common_facility.state <- Reduce(f = intersect, x = facility.state_by_year)

# Output the common chemicals
print(common_facility.state)

# Keep only common facility ids across years in the dataframe
triM <- triM %>% filter(facility.state %in% common_facility.state)
sort(unique(triM$state))
#======================================================================================================================#
### Subsetting common facility.id across years---Panelize the facility.ids
#======================================================================================================================#
# # Split the facility id column into a list of vectors by year
# facility.id_by_year <- split(triM$facility.id, triM$year)
#
# # Find the common chemicals across all states
# common_facility.id <- Reduce(f = intersect, x = facility.id_by_year)
#
# # Output the common chemicals
# print(common_facility.id)
#
# # Keep only common facility ids across years in the dataframe
# triM <- triM %>% filter(!facility.id %in% common_facility.id)
#======================================================================================================================#
### Subsetting common chemicals between the treated and the control states
#======================================================================================================================#
# Split the chemicals column into a list of vectors by state
chemicals_by_state <- split(triM$chemical.name, triM$facility.state)

# Find the common chemicals across all states
common_chemicals <- Reduce(f = intersect, x = chemicals_by_state)

# Output the common chemicals
print(common_chemicals)

# Keep common chemicals in the dataframe
triM <- triM %>% filter(chemical.name %in% common_chemicals)
sort(unique(triM$state))
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

sort(unique(triQ$state))
sort(unique(triM$facility.state))
sort(unique(triM$state_border_id))
sort(unique(triM$nearest_border))

#======================================================================================================================#
### Merging NBER-CES DATA---Manufacturing Industry Database
### Based NAICS 2012
#======================================================================================================================#
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

n_distinct(triQ.manu$facility.state)
sort(unique(triQ.manu$facility.state))

gc()
start_time <- Sys.time()
write_rds(x = triQ.manu, file = "./Data_PhD/US/BLS/triQ.manu.rds", compress = "xz")
end_time <- Sys.time()
end_time - start_time
sum_up(triQ.manu, c(total.fug.air.emissions.onsite, total.point.air.emissions.onsite, total.air.emissions.onsite))