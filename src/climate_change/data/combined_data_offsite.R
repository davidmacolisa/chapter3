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
    )
  ) %>%
  filter(
    offsite.state %in% c(
      #treated states
      "AR", "CA", "DE", "ME", "MA", "MD", "MI", "MN", "NE", "NY", "WV",
      #control states
      "GA", "IA", "ID", "IL", "IN", "KS", "KY", "NH", "NM", "NV", "NC",
      "ND", "OK", "PA", "TX", "UT", "VA", "WI", "WY"
    )
  ) %>%
  select(-c(
    pid, triid, facility.id:facility.longitude, facility.state, facility.state.code, offsite.province,
    offsite.countryid, contains(match = "potw"), contains(match = "onsite"), naics, naics.sector.code
  )
  ) %>%
  select(c(
    year, offsite.id:chemical.name, chemical.classification, unit.of.measure, contains(match = "offsite"),
    trade.secret, sanitised, entire.facility, federal.facility, govt.owned.facility, comment.type,
    comment.type.description, comment.text, classification, elemental.metal.included:chemical.ancilliary.use
  )) %>%
  data.frame()

# Making first letters uppercase
tri$offsite.city <- iconv(tri$offsite.city, to = "UTF-8")
tri$offsite.city <- tolower(tri$offsite.city)
tri$offsite.city <- stringi::stri_trans_totitle(tri$offsite.city)

tri$offsite.county <- tolower(tri$offsite.county)
tri$offsite.county <- stringi::stri_trans_totitle(tri$offsite.county)
end_time <- Sys.time()
end_time - start_time
gc()
#======================================================================================================================#
### Sorting zipcodes
#======================================================================================================================#
county_data_df <- county_df %>%
  select(county_state, county_name, fips_code) %>%
  left_join(
    y = zip_df %>% select(c(fips_code, zip_code)),
    by = c("fips_code" = "fips_code")
  )

# Count the number of characters in each zipcode
tri <- tri %>% mutate(zip.length = nchar(x = offsite.zipcode))
sort(unique(tri$zip.length))

# Group by state and count the number of unique zipcode lengths
states_with_zero_char_zip <- tri %>%
  filter(zip.length == 0) %>%
  group_by(offsite.state) %>%
  summarise(num_zero_char_zips = offsite.zipcode %>% n_distinct())

states_with_two_char_zip <- tri %>%
  filter(zip.length == 2) %>%
  group_by(offsite.state) %>%
  summarise(num_two_char_zips = offsite.zipcode %>% n_distinct())

states_with_five_char_zip <- tri %>%
  filter(zip.length == 5) %>%
  group_by(offsite.state) %>%
  summarise(num_five_char_zips = offsite.zipcode %>% n_distinct())

states_with_six_char_zip <- tri %>%
  filter(zip.length == 6) %>%
  group_by(offsite.state) %>%
  summarise(num_six_char_zips = offsite.zipcode %>% n_distinct())

states_with_nine_char_zip <- tri %>%
  filter(zip.length == 9) %>%
  group_by(offsite.state) %>%
  summarise(num_nine_char_zips = offsite.zipcode %>% n_distinct())

states_with_ten_char_zip <- tri %>%
  filter(zip.length == 10) %>%
  group_by(offsite.state) %>%
  summarise(num_ten_char_zips = offsite.zipcode %>% n_distinct())

# Count the number of states with only 0,2,5,6,9 and 10-character zipcodes
num_states_with_zero_char_zip <- nrow(states_with_zero_char_zip)
num_states_with_two_char_zip <- nrow(states_with_two_char_zip)
num_states_with_five_char_zip <- nrow(states_with_five_char_zip)
num_states_with_six_char_zip <- nrow(states_with_six_char_zip)
num_states_with_nine_char_zip <- nrow(states_with_nine_char_zip)
num_states_with_ten_char_zip <- nrow(states_with_ten_char_zip)

# Print the result
cat("Number of states with only zero-character zipcodes:", num_states_with_zero_char_zip)
cat("Number of states with only two-character zipcodes:", num_states_with_two_char_zip)
cat("Number of states with only five-character zipcodes:", num_states_with_five_char_zip)
cat("Number of states with only six-character zipcodes:", num_states_with_six_char_zip)
cat("Number of states with only nine-character zipcodes:", num_states_with_nine_char_zip)
cat("Number of states with only ten-character zipcodes:", num_states_with_ten_char_zip)
#----------------------------------------------------------------------------------------------------------------------#
## Fixing zipcodes for states having 0,2,5,6,9 and 10-character zipcodes
#----------------------------------------------------------------------------------------------------------------------#
states_with_zero_char_zip
sort(unique(tri[tri$offsite.state == "PA" & tri$zip.length == 0,]$offsite.zipcode))
sort(unique(tri[tri$offsite.state == "PA" & tri$offsite.zipcode == "",]$offsite.county))
sort(unique(tri[tri$offsite.state == "PA" & tri$offsite.county == "Lycoming",]$offsite.zipcode))
sort(unique(tri[tri$offsite.state == "PA" & tri$offsite.county == "Montgomery",]$offsite.zipcode))
sort(unique(tri[tri$zip.length == 0 &
                  tri$offsite.state == "PA" &
                  tri$offsite.county == "Montgomery",]$offsite.zipcode))
# Replace with the closest matching 5-digit zipcode to the county name
tri$offsite.zipcode <- ifelse(
  test = tri$zip.length == 0 &
    tri$offsite.state == "PA" &
    tri$offsite.county == "Lycoming",
  yes = paste0("17701", tri$offsite.zipcode),
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$zip.length == 0 &
    tri$offsite.state == "PA" &
    tri$offsite.county == "Montgomery",
  yes = paste0("18041", tri$offsite.zipcode),
  no = tri$offsite.zipcode
)

tri <- tri %>% mutate(zip.length = nchar(x = offsite.zipcode))
sort(unique(tri$zip.length))
states_with_two_char_zip
sort(unique(tri[tri$offsite.state == "MI" & tri$zip.length == 2,]$offsite.zipcode))
sort(unique(tri[tri$offsite.zipcode == "MI" & tri$zip.length == 2,]$offsite.county))
# Replace with the closest matching 5-digit zipcode to the county name
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MI" &
    tri$zip.length == 2 &
    tri$offsite.county == "Wayne",
  yes = "48101",
  no = tri$offsite.zipcode
)

tri <- tri %>% mutate(zip.length = nchar(x = offsite.zipcode))
sort(unique(tri$zip.length))
states_with_six_char_zip
sort(unique(tri[tri$offsite.state == "IL" & tri$zip.length == 6,]$offsite.zipcode))
sort(unique(tri[tri$offsite.state == "IL" & tri$zip.length == 6,]$offsite.county))
sort(unique(tri[tri$offsite.zipcode == "601290",]$offsite.county))
# Replace with the closest matching 5-digit zipcode to the county name
tri$offsite.zipcode <- ifelse(
  test = tri$zip.length == 6 & tri$offsite.county == "Kane",
  yes = "60109",
  no = tri$offsite.zipcode
)

tri <- tri %>% mutate(zip.length = nchar(x = offsite.zipcode))
sort(unique(tri$zip.length))
states_with_nine_char_zip
sort(unique(tri$offsite.state))
sort(unique(tri[tri$offsite.state == "MA" & tri$zip.length == 9,]$offsite.zipcode))
sort(unique(tri[tri$offsite.state == "MA" & tri$zip.length == 9,]$offsite.county))
sort(unique(tri[tri$offsite.zipcode == "011511022",]$offsite.county))
# Keep the first 5 characters of the zipcode
tri$offsite.zipcode <- ifelse(
  test = tri$zip.length == 9,
  yes = substr(x = tri$offsite.zipcode, start = 1, stop = 5),
  no = tri$offsite.zipcode
)

tri <- tri %>% mutate(zip.length = nchar(x = offsite.zipcode))
sort(unique(tri$zip.length))
states_with_ten_char_zip
sort(unique(tri[tri$offsite.state == "IL" & tri$zip.length == 10,]$offsite.zipcode))
sort(unique(tri[tri$offsite.zipcode == "60131-1185",]$offsite.county))
# Keep the first 5 characters of the zipcode
tri$offsite.zipcode <- ifelse(
  test = tri$zip.length == 10,
  yes = substr(x = tri$offsite.zipcode, start = 1, stop = 5),
  no = tri$offsite.zipcode
)

tri <- tri %>% mutate(zip.length = nchar(x = offsite.zipcode))
sort(unique(tri$zip.length))
sort(unique(tri$offsite.zipcode))
#======================================================================================================================#
### First merge county_data_df with tri data to get the fips_code
#======================================================================================================================#
gc()
start_time <- Sys.time()
triM <- tri %>%
  group_by(offsite.state, offsite.county) %>%
  left_join(
    y = county_data_df %>% select(-c(county_state, county_name)),
    by = c("offsite.zipcode" = "zip_code")
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
n_distinct(triM$offsite.county)
n_distinct(triM$fips_code)
n_distinct(triM$offsite.county)
sort(unique(triM$offsite.state))
#======================================================================================================================#
### Second round of sorting zipcodes
#======================================================================================================================#
triM_na <- triM[is.na(triM$fips_code),]
sort(unique(triM_na$offsite.state))
sort(unique(triM_na[triM_na$offsite.state == "CA",]$offsite.zipcode))
sort(unique(triM_na[triM_na$offsite.state == "CA",]$offsite.county))
triM_na %>%
  group_by(offsite.state, offsite.county, offsite.zipcode) %>%
  summarise(n = n()) %>%
  distinct() %>%
  print(n = nrow(.))

# AR: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "AR" & tri$offsite.zipcode == "72605",
  yes = "72531",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "AR" & tri$offsite.zipcode == "71756",
  yes = "72711",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "AR" & tri$offsite.zipcode == "71802",
  yes = "71801",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "AR" & tri$offsite.zipcode == "71613",
  yes = "71601",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "AR" & tri$offsite.zipcode == "21315",
  yes = "72310",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "AR" & tri$offsite.zipcode == "42015",
  yes = "72002",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "AR" & tri$offsite.zipcode == "92015",
  yes = "72002",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "AR" & tri$offsite.zipcode == "71790",
  yes = "71724",
  no = tri$offsite.zipcode
)

# CA: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "94451",
  yes = "94501",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "93236",
  yes = "93202",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "70222",
  yes = "90001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "90944",
  yes = "90001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "91638",
  yes = "90001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "91705",
  yes = "90001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "91725",
  yes = "90001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "91749",
  yes = "90001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "92613",
  yes = "90001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "93350",
  yes = "90001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "93521",
  yes = "90001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "97605",
  yes = "90001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "99064",
  yes = "90001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "94868",
  yes = "94901",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "90705",
  yes = "90620",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "81710",
  yes = "91701",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "CA" & tri$offsite.zipcode == "94059",
  yes = "94002",
  no = tri$offsite.zipcode
)
# tri[tri$offsite.state == "CA",]$offsite.zipcode
# tri[tri$offsite.zipcode == "93602",]$offsite.state

# DE: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "DE" & tri$offsite.zipcode == "19880",
  yes = "19701",
  no = tri$offsite.zipcode
)

# GA: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "GA" & tri$offsite.zipcode == "31202",
  yes = "31052",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "GA" & tri$offsite.zipcode == "30027",
  yes = "30236",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "GA" & tri$offsite.zipcode == "30245",
  yes = "30003",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "GA" & tri$offsite.zipcode == "31993",
  yes = "31820",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "GA" & tri$offsite.zipcode == "30267",
  yes = "30014",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "GA" & tri$offsite.zipcode == "31740",
  yes = "39836",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "GA" & tri$offsite.zipcode == "30158",
  yes = "31006",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "GA" & tri$offsite.zipcode == "31742",
  yes = "39826",
  no = tri$offsite.zipcode
)

# IL: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IL" & tri$offsite.zipcode == "40426",
  yes = "60004",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IL" & tri$offsite.zipcode == "60001",
  yes = "60004",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IL" & tri$offsite.zipcode == "60149",
  yes = "60004",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IL" & tri$offsite.zipcode == "60246",
  yes = "60004",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IL" & tri$offsite.zipcode == "60650",
  yes = "60004",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IL" & tri$offsite.zipcode == "60663",
  yes = "60004",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IL" & tri$offsite.zipcode == "60680",
  yes = "60004",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IL" & tri$offsite.zipcode == "60197",
  yes = "60101",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IL" & tri$offsite.zipcode == "52633",
  yes = "62015",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IL" & tri$offsite.zipcode == "31834",
  yes = "60932",
  no = tri$offsite.zipcode
)

# IN: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IN" & tri$offsite.zipcode == "64947",
  yes = "46932",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IN" & tri$offsite.zipcode == "46935",
  yes = "46502",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IN" & tri$offsite.zipcode == "56539",
  yes = "46502",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IN" & tri$offsite.zipcode == "46132",
  yes = "46303",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IN" & tri$offsite.zipcode == "46313",
  yes = "46303",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IN" & tri$offsite.zipcode == "46352",
  yes = "46340",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IN" & tri$offsite.zipcode == "46551",
  yes = "46530",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IN" & tri$offsite.zipcode == "47886",
  yes = "47801",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "IN" & tri$offsite.zipcode == "46882",
  yes = "46940",
  no = tri$offsite.zipcode
)

# KS: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KS" & tri$offsite.zipcode == "67946",
  yes = "67846",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KS" & tri$offsite.zipcode == "66727",
  yes = "66010",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KS" & tri$offsite.zipcode == "67504",
  yes = "67501",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KS" & tri$offsite.zipcode == "06736",
  yes = "66710",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KS" & tri$offsite.zipcode == "66726",
  yes = "66710",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KS" & tri$offsite.zipcode == "66737",
  yes = "66710",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KS" & tri$offsite.zipcode == "77636",
  yes = "66710",
  no = tri$offsite.zipcode
)

# KY: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KY" & tri$offsite.zipcode == "42142",
  yes = "42123",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KY" & tri$offsite.zipcode == "42377",
  yes = "42301",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KY" & tri$offsite.zipcode == "40326",
  yes = "40336",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KY" & tri$offsite.zipcode == "40086",
  yes = "40007",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KY" & tri$offsite.zipcode == "40290",
  yes = "40018",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KY" & tri$offsite.zipcode == "42278",
  yes = "42202",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KY" & tri$offsite.zipcode == "42026",
  yes = "42025",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KY" & tri$offsite.zipcode == "40331",
  yes = "40337",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KY" & tri$offsite.zipcode == "42135",
  yes = "42134",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "KY" & tri$offsite.zipcode == "42495",
  yes = "42437",
  no = tri$offsite.zipcode
)

# MA: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MA" & tri$offsite.zipcode == "02714",
  yes = "02048",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MA" & tri$offsite.zipcode == "04810",
  yes = "01810",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MA" & tri$offsite.zipcode == "02034",
  yes = "02018",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MA" & tri$offsite.zipcode == "02578",
  yes = "02018",
  no = tri$offsite.zipcode
)

# MD: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MD" & tri$offsite.zipcode == "21006",
  yes = "20711",
  no = tri$offsite.zipcode
)

# ME: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "ME" & tri$offsite.zipcode == "04960",
  yes = "04401",
  no = tri$offsite.zipcode
)

# MI: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MI" & tri$offsite.zipcode == "49223",
  yes = "49220",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MI" & tri$offsite.zipcode == "49585",
  yes = "49812",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MI" & tri$offsite.zipcode == "43893",
  yes = "48009",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MI" & tri$offsite.zipcode == "40735",
  yes = "49734",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MI" & tri$offsite.zipcode == "48486",
  yes = "48414",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MI" & tri$offsite.zipcode == "46211",
  yes = "48101",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MI" & tri$offsite.zipcode == "48112",
  yes = "48101",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MI" & tri$offsite.zipcode == "48277",
  yes = "48101",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MI" & tri$offsite.zipcode == "48714",
  yes = "48101",
  no = tri$offsite.zipcode
)

# MN: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MN" & tri$offsite.zipcode == "56002",
  yes = "56001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MN" & tri$offsite.zipcode == "99999",
  yes = "56115",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MN" & tri$offsite.zipcode == "55913",
  yes = "55909",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MN" & tri$offsite.zipcode == "56603",
  yes = "56003",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "MN" & tri$offsite.zipcode == "55773",
  yes = "55602",
  no = tri$offsite.zipcode
)

# NC: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NC" & tri$offsite.zipcode == "37284",
  yes = "27009",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NC" & tri$offsite.zipcode == "28176",
  yes = "28716",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NC" & tri$offsite.zipcode == "28265",
  yes = "28010",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NC" & tri$offsite.zipcode == "28225",
  yes = "28031",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NC" & tri$offsite.zipcode == "29326",
  yes = "27242",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NC" & tri$offsite.zipcode == "72343",
  yes = "27343",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NC" & tri$offsite.zipcode == "27204",
  yes = "27203",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NC" & tri$offsite.zipcode == "27323",
  yes = "27025",
  no = tri$offsite.zipcode
)

# ND: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "ND" & tri$offsite.zipcode == "58553",
  yes = "58634",
  no = tri$offsite.zipcode
)

# NE: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NE" & tri$offsite.zipcode == "68021",
  yes = "68025",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NE" & tri$offsite.zipcode == "68604",
  yes = "68007",
  no = tri$offsite.zipcode
)

# NH: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NH" & tri$offsite.zipcode == "03805",
  yes = "03032",
  no = tri$offsite.zipcode
)

# NM: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NM" & tri$offsite.zipcode == "86203",
  yes = "88201",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NM" & tri$offsite.zipcode == "88663",
  yes = "87936",
  no = tri$offsite.zipcode
)

# NV: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NV" & tri$offsite.zipcode == "85408",
  yes = "89403",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NV" & tri$offsite.zipcode == "89437",
  yes = "89440",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NV" & tri$offsite.zipcode == "89520",
  yes = "89402",
  no = tri$offsite.zipcode
)

# OK: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "OK" & tri$offsite.zipcode == "73536",
  yes = "73055",
  no = tri$offsite.zipcode
)

# NY: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NY" & tri$offsite.zipcode == "12034",
  yes = "12007",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NY" & tri$offsite.zipcode == "14909",
  yes = "14814",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NY" & tri$offsite.zipcode == "13503",
  yes = "13322",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NY" & tri$offsite.zipcode == "00000",
  yes = "13030",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NY" & tri$offsite.zipcode == "13442",
  yes = "13042",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NY" & tri$offsite.zipcode == "13503",
  yes = "13042",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NY" & tri$offsite.zipcode == "13221",
  yes = "13020",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "NY" & tri$offsite.zipcode == "14811",
  yes = "14529",
  no = tri$offsite.zipcode
)

# OK: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "OK" & tri$offsite.zipcode == "74362",
  yes = "74330",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "OK" & tri$offsite.zipcode == "74502",
  yes = "74425",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "OK" & tri$offsite.zipcode == "74785",
  yes = "74801",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "OK" & tri$offsite.zipcode == "74176",
  yes = "74008",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "OK" & tri$offsite.zipcode == "73680",
  yes = "73717",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "OK" & tri$offsite.zipcode == "73862",
  yes = "73717",
  no = tri$offsite.zipcode
)

# PA: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "15405",
  yes = "15006",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "19874",
  yes = "18039",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "16062",
  yes = "16001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "19280",
  yes = "19301",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "17448",
  yes = "16822",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "15471",
  yes = "15401",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "14334",
  yes = "15310",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "16767",
  yes = "15711",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "16103",
  yes = "16101",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "17607",
  yes = "17003",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "19011",
  yes = "18011",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "14317",
  yes = "15004",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "15069",
  yes = "15012",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "15264",
  yes = "15012",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "PA" & tri$offsite.zipcode == "17379",
  yes = "17019",
  no = tri$offsite.zipcode
)

# TX: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "77151",
  yes = "77422",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "77513",
  yes = "77422",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "75031",
  yes = "75002",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "75284",
  yes = "75001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "75320",
  yes = "75001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "50084",
  yes = "75101",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "75120",
  yes = "75101",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "77213",
  yes = "77001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "77216",
  yes = "77001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "77671",
  yes = "75124",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "75653",
  yes = "75124",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "75728",
  yes = "75124",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "77540",
  yes = "77613",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "77604",
  yes = "77613",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "77643",
  yes = "77613",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "77646",
  yes = "77613",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "85279",
  yes = "79329",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "78388",
  yes = "78330",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "87407",
  yes = "78330",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "77631",
  yes = "77611",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "TX" & tri$offsite.zipcode == "75653",
  yes = "75652",
  no = tri$offsite.zipcode
)

# UT: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "UT" & tri$offsite.zipcode == "00000",
  yes = "84301",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "UT" & tri$offsite.zipcode == "34812",
  yes = "84301",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "UT" & tri$offsite.zipcode == "81404",
  yes = "84006",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "UT" & tri$offsite.zipcode == "84209",
  yes = "84022",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "UT" & tri$offsite.zipcode == "94029",
  yes = "84022",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "UT" & tri$offsite.zipcode == "84650",
  yes = "84003",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "UT" & tri$offsite.zipcode == "84903",
  yes = "84003",
  no = tri$offsite.zipcode
)

# VA: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "VA" & tri$offsite.zipcode == "30671",
  yes = "23801",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "VA" & tri$offsite.zipcode == "24116",
  yes = "24415",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "VA" & tri$offsite.zipcode == "24143",
  yes = "24060",
  no = tri$offsite.zipcode
)

# WI: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "WI" & tri$offsite.zipcode == "54131",
  yes = "54115",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "WI" & tri$offsite.zipcode == "54219",
  yes = "53014",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "WI" & tri$offsite.zipcode == "53603",
  yes = "53015",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "WI" & tri$offsite.zipcode == "54221",
  yes = "53015",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "WI" & tri$offsite.zipcode == "54134",
  yes = "54101",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "WI" & tri$offsite.zipcode == "54131",
  yes = "54106",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "WI" & tri$offsite.zipcode == "54219",
  yes = "53001",
  no = tri$offsite.zipcode
)
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "WI" & tri$offsite.zipcode == "63007",
  yes = "53005",
  no = tri$offsite.zipcode
)

# WV: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$offsite.zipcode <- ifelse(
  test = tri$offsite.state == "WV" & tri$offsite.zipcode == "26262",
  yes = "54611",
  no = tri$offsite.zipcode
)
#======================================================================================================================#
### Final merge county_data_df with tri data to get the fips_code
#======================================================================================================================#
gc()
start_time <- Sys.time()
triM <- tri %>%
  group_by(offsite.state, offsite.county) %>%
  left_join(
    y = county_data_df %>% select(-c(county_state, county_name)),
    by = c("offsite.zipcode" = "zip_code")
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
sum(is.na(triM$offsite.zipcode))
n_distinct(triM$offsite.id)
n_distinct(triM$offsite.facility.id)
n_distinct(triM$fips_code)
n_distinct(triM$offsite.county)
sort(unique(triM$offsite.state))
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
triM_fac <- triM %>% filter(!offsite.facility.id %in% common_offsite.fac.id)
#======================================================================================================================#
### Keeping only the common chemicals between the treated and the control states---Panelize the chemicals
#======================================================================================================================#
gc()
# Split the chemicals column into a list of vectors by state
chemicals_by_state <- split(triM$chemical.name, triM$offsite.state)

# Find the common chemicals across all states
common_chemicals <- Reduce(f = intersect, x = chemicals_by_state)

# Output the common chemicals
print(common_chemicals)

# Keep common chemicals in the dataframe
triM <- triM %>% filter(chemical.name %in% common_chemicals)
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
  )
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
glimpse(triQc_on)

gc()
start_time <- Sys.time()
write_rds(x = triQc_off, file = "./Data_PhD/US/BLS/offsite/triQc_off.rds", compress = "xz")
end_time <- Sys.time()
end_time - start_time
gc()
sum_up(triQc_off, c(total.underground.injection.offsite))
gc()
#======================================================================================================================#