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
### Loading US county and zipcode data
#======================================================================================================================#
data(county_df, package = "usgeogr")
county_df <- county_df %>%
  select(fips_code, county_name, county_state) %>%
  data.frame()
county_df$county_name <- gsub(pattern = "\\b(\\w+)$", replacement = "", x = county_df$county_name)

data(zip_df, package = "usgeogr")
zip_df <- zip_df %>% data.frame()
#======================================================================================================================#
### Loading Data: TRI---Form R from EPA
#======================================================================================================================#
filepath <- "./Data_PhD/US/EPA/AQS/toxic_release_inventory/triR.rds"
start_time <- Sys.time()
tri <- readRDS(file = filepath) %>%
  filter(year >= 2011 & year <= 2017) %>%
  filter(
    facility.state %in% c(
      #treated states
      "AR", "CA", "DE", "ME", "MA", "MD", "MI", "MN", "NE", "NY", "WV",
      #control states
      "GA", "IA", "ID", "IL", "IN", "KS", "KY", "NH", "NM", "NV", "NC",
      "ND", "OK", "PA", "TX", "UT", "VA", "WI", "WY"
    ) &
      offsite.state %in% c(
        "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
        "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
        "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
        "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
        "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
      )
  ) %>%
  select(-c(
    pid, triid, facility.state.code, offsite.province, offsite.countryid, naics, naics.sector.code
  )
  ) %>%
  select(c(
    year, facility.id:facility.longitude, facility.state, offsite.id:chemical.name, chemical.classification,
    unit.of.measure, contains(match = "offsite"), trade.secret, sanitised, entire.facility, federal.facility,
    govt.owned.facility, comment.type, comment.type.description, comment.text, classification,
    elemental.metal.included:chemical.ancilliary.use
  )) %>%
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
end_time <- Sys.time()
end_time - start_time
gc()
#======================================================================================================================#
### Sorting onsite zipcodes
#======================================================================================================================#
county_data_df <- county_df %>%
  select(county_state, county_name, fips_code) %>%
  left_join(
    y = zip_df %>% select(c(fips_code, zip_code)),
    by = c("fips_code" = "fips_code")
  )

# Count the number of characters in each zipcode
tri <- tri %>% mutate(zip.length = nchar(x = facility.zipcode))
sort(unique(tri$zip.length))

# Group by state and count the number of unique zipcode lengths
states_with_four_char_zip <- tri %>%
  filter(zip.length == 4) %>%
  group_by(facility.state) %>%
  summarise(num_four_char_zips = facility.zipcode %>% n_distinct())

states_with_five_char_zip <- tri %>%
  filter(zip.length == 5) %>%
  group_by(facility.state) %>%
  summarise(num_five_char_zips = facility.zipcode %>% n_distinct())

states_with_eight_char_zip <- tri %>%
  filter(zip.length == 8) %>%
  group_by(facility.state) %>%
  summarise(num_eight_char_zips = facility.zipcode %>% n_distinct())

states_with_nine_char_zip <- tri %>%
  filter(zip.length == 9) %>%
  group_by(facility.state) %>%
  summarise(num_nine_char_zips = facility.zipcode %>% n_distinct())

# Count the number of states with only 4,5,8 and 9-character zipcodes
num_states_with_four_char_zip <- nrow(states_with_four_char_zip)
num_states_with_five_char_zip <- nrow(states_with_five_char_zip)
num_states_with_eight_char_zip <- nrow(states_with_eight_char_zip)
num_states_with_nine_char_zip <- nrow(states_with_nine_char_zip)

# Print the result
cat("Number of states with only four-character zipcodes:", num_states_with_four_char_zip)
cat("Number of states with only five-character zipcodes:", num_states_with_five_char_zip)
cat("Number of states with only eight-character zipcodes:", num_states_with_eight_char_zip)
cat("Number of states with only nine-character zipcodes:", num_states_with_nine_char_zip)
#----------------------------------------------------------------------------------------------------------------------#
### Fixing zipcodes for states having 4,5,8 and 9-character zipcodes---Onsite
#----------------------------------------------------------------------------------------------------------------------#
states_with_four_char_zip
sort(unique(tri[tri$facility.state == "NH" & tri$zip.length == 4,]$facility.zipcode))
tri[tri$facility.zipcode == "38596",]$facility.county
# Add 0 to the front of the zipcode to make a 5-digit zipcode
tri$facility.zipcode <- ifelse(
  test = tri$zip.length == 4,
  yes = paste0("0", tri$facility.zipcode),
  no = tri$facility.zipcode
)

tri <- tri %>% mutate(zip.length = nchar(x = facility.zipcode))
sort(unique(tri$zip.length))
states_with_eight_char_zip
sort(unique(tri[tri$facility.state == "ME" & tri$zip.length == 8,]$facility.zipcode))
tri[tri$facility.zipcode == "46056031",]$facility.county
# Keep the first 4 characters of the zipcode
tri$facility.zipcode <- ifelse(
  test = tri$zip.length == 8,
  yes = substr(x = tri$facility.zipcode, start = 1, stop = 4),
  no = tri$facility.zipcode
)
# Count the number of characters in each zipcode
tri <- tri %>% mutate(zip.length = nchar(x = facility.zipcode))
sort(unique(tri$zip.length))
# Add 0 to the front of the zipcode
tri$facility.zipcode <- ifelse(
  test = tri$zip.length == 4,
  yes = paste0("0", tri$facility.zipcode),
  no = tri$facility.zipcode
)

tri <- tri %>% mutate(zip.length = nchar(x = facility.zipcode))
states_with_nine_char_zip
sort(unique(tri[tri$facility.state == "CA" & tri$zip.length == 9,]$facility.zipcode))
tri[tri$facility.zipcode == "900164409",]$facility.county
# Keep the first 5 characters of the zipcode
tri$facility.zipcode <- ifelse(
  test = tri$zip.length == 9,
  yes = substr(x = tri$facility.zipcode, start = 1, stop = 5),
  no = tri$facility.zipcode
)

tri <- tri %>% mutate(zip.length = nchar(x = facility.zipcode))
sort(unique(tri$zip.length))
sort(unique(tri$facility.zipcode))
#======================================================================================================================#
### First merge county_data_df with tri data to get the fips_code for onsite
#======================================================================================================================#
gc()
start_time <- Sys.time()
triM <- tri %>%
  group_by(facility.state, facility.county) %>%
  mutate(
    facility.longitude = as.numeric(facility.longitude),
    facility.latitude = as.numeric(facility.latitude)
  ) %>%
  left_join(
    y = county_data_df %>% select(-c(county_state, county_name)),
    by = c("facility.zipcode" = "zip_code")
  ) %>%
  left_join(
    y = county_df %>% select(-c(county_state, county_name)),
    by = c("fips_code" = "fips_code")
  ) %>%
  data.frame()
end_time <- Sys.time()
end_time - start_time
gc()

sum(is.na(triM$fips_code))
n_distinct(triM$facility.id)
n_distinct(triM$fips_code)
n_distinct(triM$facility.county)
sort(unique(triM$facility.state))
#======================================================================================================================#
### Second round of sorting onsite zipcodes
#======================================================================================================================#
triM_na <- triM[is.na(triM$fips_code),]
sort(unique(triM_na$facility.state))
triM_na %>%
  group_by(facility.state, facility.county, facility.zipcode) %>%
  summarise(n = n()) %>%
  distinct() %>%
  print(n = nrow(.))

# IA: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "IA" & tri$facility.zipcode == "52733",
  yes = "52732",
  no = tri$facility.zipcode
)

# ID: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "ID" & tri$facility.zipcode == "83415",
  yes = "83255",
  no = tri$facility.zipcode
)

# IN: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "IN" & tri$facility.zipcode == "47549",
  yes = "47580",
  no = tri$facility.zipcode
)

# MI: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "MI" & tri$facility.zipcode == "49739",
  yes = "49738",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "MI" & tri$facility.zipcode == "48551",
  yes = "48550",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "MI" & tri$facility.zipcode == "48556",
  yes = "48550",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "MI" & tri$facility.zipcode == "49335",
  yes = "49588",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "MI" & tri$facility.zipcode == "48686",
  yes = "48674",
  no = tri$facility.zipcode
)

# NV: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "NV" & tri$facility.zipcode == "89466",
  yes = "89883",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "NV" & tri$facility.zipcode == "89315",
  yes = "89319",
  no = tri$facility.zipcode
)

# OK: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "OK" & tri$facility.zipcode == "73536",
  yes = "73533",
  no = tri$facility.zipcode
)

# PA: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "PA" & tri$facility.zipcode == "16531",
  yes = "16565",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "PA" & tri$facility.zipcode == "16107",
  yes = "16172",
  no = tri$facility.zipcode
)

# TX: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "TX" & tri$facility.zipcode == "75507",
  yes = "75501",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "TX" & tri$facility.zipcode == "77641",
  yes = "77613",
  no = tri$facility.zipcode
)

# WI: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "WI" & tri$facility.zipcode == "54221",
  yes = "53015",
  no = tri$facility.zipcode
)

#======================================================================================================================#
### Second merge county_data_df with tri data to get the fips_code onsite
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
    y = county_data_df %>% select(-c(county_state, county_name)),
    by = c("facility.zipcode" = "zip_code")
  ) %>%
  left_join(
    y = county_df %>% select(-c(county_state, county_name)),
    by = c("fips_code" = "fips_code")
  ) %>%
  data.frame()
end_time <- Sys.time()
end_time - start_time
gc()

sum(is.na(triM$fips_code))
triM_na <- triM[is.na(triM$fips_code),]
sum(is.na(triM$facility.zipcode))
n_distinct(triM$facility.id)
n_distinct(triM$fips_code)
n_distinct(triM$facility.county)
sort(unique(triM$facility.state))

#======================================================================================================================#
### Third Sorting of the onsite zipcodes
####======================================================================================================================#
triM_na %>%
  group_by(facility.state, facility.county, facility.zipcode) %>%
  summarise(n = n()) %>%
  distinct() %>%
  print(n = nrow(.))

# MI: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "MI" & tri$facility.zipcode == "49355",
  yes = "49319",
  no = tri$facility.zipcode
)

# NV: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "NV" & tri$facility.zipcode == "89446",
  yes = "89822",
  no = tri$facility.zipcode
)

#======================================================================================================================#
### Final merge county_data_df with tri data to get the fips_code onsite
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
    y = county_data_df %>% select(-c(county_state, county_name)),
    by = c("facility.zipcode" = "zip_code")
  ) %>%
  left_join(
    y = county_df %>% select(-c(county_state, county_name)),
    by = c("fips_code" = "fips_code")
  ) %>%
  data.frame()
end_time <- Sys.time()
end_time - start_time
gc()

sum(is.na(triM$fips_code))
#======================================================================================================================#
### Sorting offsite zipcodes
#======================================================================================================================#
# Count the number of characters in each zipcode
triM <- triM %>% mutate(offsite.zip.length = nchar(x = offsite.zipcode))
sort(unique(triM$offsite.zip.length))

# Group by state and count the number of unique zipcode lengths
states_with_zero_char_zip <- triM %>%
  filter(offsite.zip.length == 0) %>%
  group_by(offsite.state) %>%
  summarise(num_zero_char_zips = offsite.zipcode %>% n_distinct())

states_with_four_char_zip <- triM %>%
  filter(offsite.zip.length == 4) %>%
  group_by(offsite.state) %>%
  summarise(num_four_char_zips = offsite.zipcode %>% n_distinct())

states_with_five_char_zip <- triM %>%
  filter(offsite.zip.length == 5) %>%
  group_by(offsite.state) %>%
  summarise(num_five_char_zips = offsite.zipcode %>% n_distinct())

states_with_six_char_zip <- triM %>%
  filter(offsite.zip.length == 6) %>%
  group_by(offsite.state) %>%
  summarise(num_six_char_zips = offsite.zipcode %>% n_distinct())

states_with_nine_char_zip <- triM %>%
  filter(offsite.zip.length == 9) %>%
  group_by(offsite.state) %>%
  summarise(num_nine_char_zips = offsite.zipcode %>% n_distinct())

states_with_ten_char_zip <- triM %>%
  filter(offsite.zip.length == 10) %>%
  group_by(offsite.state) %>%
  summarise(num_ten_char_zips = offsite.zipcode %>% n_distinct())

# Count the number of states with only 0,2,5,6,9 and 10-character zipcodes
num_states_with_zero_char_zip <- nrow(states_with_zero_char_zip)
num_states_with_four_char_zip <- nrow(states_with_four_char_zip)
num_states_with_five_char_zip <- nrow(states_with_five_char_zip)
num_states_with_six_char_zip <- nrow(states_with_six_char_zip)
num_states_with_nine_char_zip <- nrow(states_with_nine_char_zip)
num_states_with_ten_char_zip <- nrow(states_with_ten_char_zip)

# Print the result
cat("Number of states with only zero-character zipcodes:", num_states_with_zero_char_zip)
cat("Number of states with only two-character zipcodes:", num_states_with_four_char_zip)
cat("Number of states with only five-character zipcodes:", num_states_with_five_char_zip)
cat("Number of states with only six-character zipcodes:", num_states_with_six_char_zip)
cat("Number of states with only nine-character zipcodes:", num_states_with_nine_char_zip)
cat("Number of states with only ten-character zipcodes:", num_states_with_ten_char_zip)
#----------------------------------------------------------------------------------------------------------------------#
## Fixing zipcodes for states having 0,2,5,6,9 and 10-character offsite zipcodes
#----------------------------------------------------------------------------------------------------------------------#
states_with_zero_char_zip
sort(unique(triM[triM$offsite.state == "PA" & triM$offsite.zip.length == 0,]$offsite.zipcode))
sort(unique(triM[triM$offsite.state == "PA" & triM$offsite.zipcode == "",]$offsite.county))
sort(unique(triM[triM$offsite.state == "PA" & triM$offsite.county == "Lycoming",]$offsite.zipcode))
sort(unique(triM[triM$offsite.state == "PA" & triM$offsite.county == "Montgomery",]$offsite.zipcode))
sort(unique(triM[triM$offsite.zip.length == 0 &
                   triM$offsite.state == "PA" &
                   triM$offsite.county == "Montgomery",]$offsite.zipcode))
# Replace with the closest matching 5-digit zipcode to the county name
triM$offsite.zipcode <- ifelse(
  test = triM$offsite.zip.length == 0 &
    triM$offsite.state == "PA" &
    triM$offsite.county == "Lycoming",
  yes = paste0("17701", triM$offsite.zipcode),
  no = triM$offsite.zipcode
)
triM$offsite.zipcode <- ifelse(
  test = triM$offsite.zip.length == 0 &
    triM$offsite.state == "PA" &
    triM$offsite.county == "Montgomery",
  yes = paste0("18041", triM$offsite.zipcode),
  no = triM$offsite.zipcode
)

triM <- triM %>% mutate(offsite.zip.length = nchar(x = offsite.zipcode))
sort(unique(triM$offsite.zip.length))
states_with_four_char_zip
sort(unique(triM[triM$offsite.state == "TN" & triM$offsite.zip.length == 4,]$offsite.zipcode))
sort(unique(triM[triM$offsite.state == "TN" & triM$offsite.zip.length == 4,]$offsite.county))
# Replace with the closest matching 5-digit zipcode to the county name
triM$offsite.zipcode <- ifelse(
  test = triM$offsite.state == "TN" &
    triM$offsite.zip.length == 4 &
    triM$offsite.county == "Shelby",
  yes = "37501",
  no = triM$offsite.zipcode
)

triM <- triM %>% mutate(offsite.zip.length = nchar(x = offsite.zipcode))
sort(unique(triM$offsite.zip.length))
states_with_six_char_zip
sort(unique(triM[triM$offsite.state == "IL" & triM$offsite.zip.length == 6,]$offsite.zipcode))
sort(unique(triM[triM$offsite.state == "IL" & triM$offsite.zip.length == 6,]$offsite.county))
sort(unique(triM[triM$offsite.zipcode == "601290",]$offsite.county))
# Replace with the closest matching 5-digit zipcode to the county name
triM$offsite.zipcode <- ifelse(
  test = triM$offsite.zip.length == 6 & triM$offsite.county == "Kane",
  yes = "60109",
  no = triM$offsite.zipcode
)
###
triM <- triM %>% mutate(offsite.zip.length = nchar(x = offsite.zipcode))
sort(unique(triM$offsite.zip.length))
states_with_nine_char_zip
sort(unique(triM$offsite.state))
sort(unique(triM[triM$offsite.state == "AL" & triM$offsite.zip.length == 9,]$offsite.zipcode))
sort(unique(triM[triM$offsite.state == "AL" & triM$offsite.zip.length == 9,]$offsite.county))
sort(unique(triM[triM$offsite.zipcode == "356346523",]$offsite.county))
# Keep the first 5 characters of the zipcode
triM$offsite.zipcode <- ifelse(
  test = triM$offsite.zip.length == 9,
  yes = substr(x = triM$offsite.zipcode, start = 1, stop = 5),
  no = triM$offsite.zipcode
)

triM <- triM %>% mutate(offsite.zip.length = nchar(x = offsite.zipcode))
sort(unique(triM$offsite.zip.length))
states_with_ten_char_zip
sort(unique(triM[triM$offsite.state == "IL" & triM$offsite.zip.length == 10,]$offsite.zipcode))
sort(unique(triM[triM$offsite.zipcode == "60131-1185",]$offsite.county))
# Keep the first 5 characters of the zipcode
triM$offsite.zipcode <- ifelse(
  test = triM$offsite.zip.length == 10,
  yes = substr(x = triM$offsite.zipcode, start = 1, stop = 5),
  no = triM$offsite.zipcode
)

triM <- triM %>% mutate(offsite.zip.length = nchar(x = offsite.zipcode))
sort(unique(triM$offsite.zip.length))
sort(unique(triM$offsite.zipcode))
sum(is.na(triM$offsite.zipcode))

A <- triM %>% filter(offsite.zip.length == 5)
triM <- A
#======================================================================================================================#
### Keeping only common offsite states across years---Panelize the offsite.state
#======================================================================================================================#
gc()
# Split the offsite id column into a list of vectors by year
offsite.state_by_year <- split(triM$offsite.state, triM$year)

# Find the common chemicals across all states
common_offsite.state <- Reduce(f = intersect, x = offsite.state_by_year)

# Output the common chemicals
print(common_offsite.state)

# Keep only common offsite ids across years in the dataframe
triM <- triM %>% filter(offsite.state %in% common_offsite.state)
gc()
#======================================================================================================================#
### Keeping only common offsite.facility.id across years---Panelize the offsite.facility.ids
#======================================================================================================================#
# Split the offsite id column into a list of vectors by year
offsite.fac.id_by_year <- split(triM$offsite.facility.id, triM$year)

# Find the common chemicals across all states
common_offsite.fac.id <- Reduce(f = intersect, x = offsite.fac.id_by_year)

# Output the common chemicals
print(common_offsite.fac.id)

# Keep only common offsite facility ids across years in the dataframe
triM_fac_off <- triM %>% filter(!offsite.facility.id %in% common_offsite.fac.id)
#======================================================================================================================#
### Keeping only the common chemicals between the treated and the control states---Panelize the chemicals
#======================================================================================================================#
gc()
# Split the chemicals column into a list of vectors by state
# chemicals_by_state <- split(triM$chemical.name, triM$offsite.state)
#
# # Find the common chemicals across all states
# common_chemicals <- Reduce(f = intersect, x = chemicals_by_state)
#
# # Output the common chemicals
# print(common_chemicals)
#
# # Keep common chemicals in the dataframe
# triM <- triM %>% filter(chemical.name %in% common_chemicals)
gc()
#======================================================================================================================#
### Merge ghgp with tri, both from EPA
#======================================================================================================================#
ghgp <- read_rds(file = "./Data_PhD/US/EPA/AQS/ghg/ghgp.rds") %>% data.frame()

# Remove last word in county labels
ghgp$facility.county <- sub(pattern = "\\s+\\w+$", replacement = "", ghgp$facility.county)

ghgp <- ghgp %>%
  left_join(
    y = zip_df %>% select(fips_code, zip_code),
    by = c("facility.zipcode" = "zip_code")
  )

start_time <- Sys.time()
triM_ghgp <- triM %>%
  right_join(
    y = ghgp %>%
      mutate(year = as.numeric(year), ghgp.unit = "tons") %>%
      select(c(year, facility.frs.id, fips_code, ghgp.unit, naics.code, co2.emissions,
               methane.emissions, nitrousoxide.emissions)),
    by = c("year" = "year", "fips_code" = "fips_code", "offsite.facility.id" = "facility.frs.id")
  )
end_time <- Sys.time()
end_time - start_time
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
#======================================================================================================================#
### Assets
#======================================================================================================================#
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
#   rename(zip_code = zipba, offsite.name = name) %>%
#   select(c(offsite.name, zip_code, sic, assets, year)) %>%
#   left_join(
#     y = zip_df %>% select(c(fips_code, zip_code)),
#     by = ("zip_code" = "zip_code")
#   ) %>%
#   data.frame()
#======================================================================================================================#
### Loading QCEW---County Levels from BLS
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
      mutate(year = as.numeric(year)),
    by = c("fips_code" = "fips_code", "offsite.county" = "county_name", "offsite.state" = "county_state",
           "year" = "year"),
  ) %>%
  left_join(
    y = qcew %>%
      select(c(year, fips_code, own_code, naics.code, annual_avg_estabs:avg_annual_pay)) %>%
      mutate(year = as.numeric(year)),
    by = c("naics.code" = "naics.code", "fips_code" = "fips_code", "year" = "year"),
  ) %>%
  data.frame()
end_time <- Sys.time()
end_time - start_time
gc()

sort(unique(triM$offsite.state))
#======================================================================================================================#
### Merging NBER-CES DATA---Manufacturing Industry Database
### Based on NAICS 2012
#======================================================================================================================#
gc()
start_time <- Sys.time()
triQ_manu <- triQ %>%
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
  data.frame()
end_time <- Sys.time()
end_time - start_time
gc()

n_distinct(triQ_manu$offsite.state)
sort(unique(triQ_manu$offsite.state))
glimpse(triQ_manu)
triQ_manu <- triQ_manu %>%
  mutate(
    trade.secret = as.character(trade.secret),
    sanitised = as.character(sanitised),
    entire.facility = as.character(entire.facility),
    federal.facility = as.character(federal.facility),
    govt.owned.facility = as.character(govt.owned.facility),
    clean.air.act.chems = as.character(clean.air.act.chems),
    carcinogenic.chems = as.character(carcinogenic.chems),
    produced.chem.facility = as.character(produced.chem.facility),
    imported.chem.facility = as.character(imported.chem.facility),
    pi.chem.facility = as.character(pi.chem.facility),
    metal.restrict.tri = as.character(metal.restrict.tri),
    chemical.formulation.component = as.character(chemical.formulation.component),
    chemical.article.component = as.character(chemical.article.component),
    chemical.manufacturing.aid = as.character(chemical.manufacturing.aid),
    chemical.ancilliary.use = as.character(chemical.ancilliary.use),
    zip.length = as.character(zip.length),
    personal_income = as.numeric(personal_income),
    gdp = as.numeric(gdp),
    compensation_to_employees = as.numeric(compensation_to_employees),
    regional_price_parity = as.numeric(regional_price_parity),
    annual_avg_estabs = as.numeric(annual_avg_estabs),
    annual_avg_emplvl = as.numeric(annual_avg_emplvl),
    total_annual_wages = as.numeric(total_annual_wages),
    taxable_annual_wages = as.numeric(taxable_annual_wages),
    annual_contributions = as.numeric(annual_contributions),
    annual_avg_wkly_wage = as.numeric(annual_avg_wkly_wage),
    avg_annual_pay = as.numeric(avg_annual_pay)
  ) %>%
  select(-c(trade.secret, sanitised, federal.facility, elemental.metal.included, chemical.intermediate.uses, pfas.chems))
#======================================================================================================================#
### Merging triQ_manu with the US shapfile for border county design.
# Merging to fac_county_df or fac_states_df for county level and state level analysis.
# Keep only onsite variables
#======================================================================================================================#
# Border-County Design: Onsite
triQc_off <- triQ_manu %>%
  right_join(
    y = fac_county_df,
    by = c("fips_code" = "fips_code")
  )
glimpse(triQc_off)
triQc_na <- triQc_off[is.na(triQc_off$offsite.zipcode),]
triQc_off <- triQc_off[complete.cases(triQc_off$offsite.zipcode),]
sum(is.na(triQc_off))
sum(is.na(triQc_off$fips_code))
sum(is.na(triQc_off$offsite.zipcode))
sum(is.na(triQc_off$offsite.city))
n_distinct(triQc_off$fips_code)
n_distinct(triQc_off$offsite.facility.id)
n_distinct(triQc_off$state.code)
n_distinct(triQc_off$offsite.state)
n_distinct(triQc_off$chemical.id)
sort(unique(triQc_off$state))
glimpse(triQc_off)

gc()
start_time <- Sys.time()
write_rds(x = triQc_off, file = "./Data_PhD/US/BLS/offsite/triQc_off.rds", compress = "xz")
end_time <- Sys.time()
end_time - start_time
gc()
sum_up(triQc_off, c(total.underground.injection.offsite, total.fug.air.emissions))
gc()
#======================================================================================================================#