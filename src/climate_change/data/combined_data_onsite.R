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
  select(
    -c(facility.state.code, naics, naics.sector.code, offsite.province, offsite.countryid, mixture)
  ) %>%
  data.frame()

sort(unique(tri$facility.state))
sort(unique(tri$offsite.state))
sort(unique(tri$potw.state))


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

tri$potw.city <- iconv(tri$potw.city, to = "UTF-8")
tri$potw.city <- tolower(tri$potw.city)
tri$potw.city <- stringi::stri_trans_totitle(tri$potw.city)

tri$potw.county <- tolower(tri$potw.county)
tri$potw.county <- stringi::stri_trans_totitle(tri$potw.county)
end_time <- Sys.time()
end_time - start_time

na_columns <- colnames(tri)[colSums(is.na(tri)) > 0]

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

sort(unique(tri$facility.zipcode))
sort(unique(tri[tri$facility.state == "MA",]$facility.zipcode))
sort(unique(tri[tri$facility.state == "ME",]$facility.zipcode))
tri[tri$facility.zipcode == "982783500",]$facility.county
tri[tri$facility.zipcode == "3904",]$facility.county

# Count the number of characters in each zipcode
tri <- tri %>% mutate(zip.length = nchar(x = facility.zipcode))
sort(unique(tri$zip.length))
#tri <- tri[tri$zip.length==5,]

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
sort(unique(tri[tri$facility.state == "NH",]$facility.zipcode))
tri[tri$facility.zipcode == "3839",]$facility.county
# Add 0 to the front of the zipcode to make a 5-digit zipcode
tri$facility.zipcode <- ifelse(
  test = tri$zip.length == 4,
  yes = paste0("0", tri$facility.zipcode),
  no = tri$facility.zipcode
)

states_with_eight_char_zip
sort(unique(tri[tri$facility.state == "ME",]$facility.zipcode))
tri[tri$facility.zipcode == "46056031",]$facility.county
# Keep the first 4 characters of the zipcode
tri$facility.zipcode <- ifelse(
  test = tri$zip.length == 8,
  yes = substr(x = tri$facility.zipcode, start = 1, stop = 4),
  no = tri$facility.zipcode
)
# Count the number of characters in each zipcode
tri <- tri %>% mutate(zip.length = nchar(x = facility.zipcode))
# Add 0 to the front of the zipcode
tri$facility.zipcode <- ifelse(
  test = tri$zip.length == 4,
  yes = paste0("0", tri$facility.zipcode),
  no = tri$facility.zipcode
)

states_with_nine_char_zip
sort(unique(tri[tri$facility.state == "AL",]$facility.zipcode))
tri[tri$facility.zipcode == "359012027",]$facility.county
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
n_distinct(tri$facility.zipcode)
n_distinct(tri$facility.id)
n_distinct(county_data_df$zip_code)
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
triM_na[triM_na$facility.state == "CA",]$facility.zipcode
triM_na[triM_na$facility.state == "CA",]$facility.county
triM_na %>%
  group_by(facility.state, facility.county, facility.zipcode) %>%
  summarise(n = n()) %>%
  distinct() %>%
  print(n = nrow(.))

# CA: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "CA" & tri$facility.zipcode == "93607",
  yes = "93602",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "CA" & tri$facility.zipcode == "92714",
  yes = "92711",
  no = tri$facility.zipcode
)
# tri[tri$facility.state == "CA",]$facility.zipcode
# tri[tri$facility.zipcode == "93602",]$facility.state

# GA: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "GA" & tri$facility.zipcode == "30001",
  yes = "30103",
  no = tri$facility.zipcode
)

# IA: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "IA" & tri$facility.zipcode == "52733",
  yes = "52777",
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
  yes = "47513",
  no = tri$facility.zipcode
)

# KY: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "KY" & tri$facility.zipcode == "40384",
  yes = "40347",
  no = tri$facility.zipcode
)

# MI: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "MI" & tri$facility.zipcode == "49739",
  yes = "49733",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "MI" & tri$facility.zipcode == "49811",
  yes = "49801",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "MI" & tri$facility.zipcode == "48551",
  yes = "48411",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "MI" & tri$facility.zipcode == "48556",
  yes = "48411",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "MI" & tri$facility.zipcode == "49355",
  yes = "49301",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "MI" & tri$facility.zipcode == "48686",
  yes = "48618",
  no = tri$facility.zipcode
)
# MN: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "MN" & tri$facility.zipcode == "55190",
  yes = "55910",
  no = tri$facility.zipcode
)

# NV: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "NV" & tri$facility.zipcode == "89446",
  yes = "89801",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "NV" & tri$facility.zipcode == "89315",
  yes = "89301",
  no = tri$facility.zipcode
)

# OK: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "OK" & tri$facility.zipcode == "73536",
  yes = "73055",
  no = tri$facility.zipcode
)

# PA: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "PA" & tri$facility.zipcode == "16531",
  yes = "16401",
  no = tri$facility.zipcode
)
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "PA" & tri$facility.zipcode == "16107",
  yes = "16101",
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

# WY: Replace the zipcode with the matching zipcode for the same county name in county_data_df
tri$facility.zipcode <- ifelse(
  test = tri$facility.state == "WY" & tri$facility.zipcode == "82717",
  yes = "82716",
  no = tri$facility.zipcode
)
#======================================================================================================================#
### Final merge county_data_df with tri data to get the fips_code onsite
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
sum(is.na(triM$facility.zipcode))
n_distinct(triM$facility.id)
n_distinct(triM$fips_code)
n_distinct(triM$facility.county)
sort(unique(triM$facility.state))
#======================================================================================================================#
### Sorting offsite and potw zipcodes
#======================================================================================================================#
# Count the number of characters in each zipcode
triM <- triM %>% mutate(offsite.zip.length = nchar(x = offsite.zipcode))
triM <- triM %>% mutate(potw.zip.length = nchar(x = potw.zipcode))

sort(unique(triM$offsite.zip.length))
sort(unique(triM$potw.zip.length))
#======================================================================================================================#
### Keeping only common facility states across years---Panelize the facility.state
#======================================================================================================================#
gc()
# Split the facility states column into a list of vectors by year
facility.state_by_year <- split(triM$facility.state, triM$year)

# Find the common states across all years
common_facility.state <- Reduce(f = intersect, x = facility.state_by_year)

# Output the common states
print(sort(common_facility.state))

# Keep only common facility ids across years in the dataframe
triM <- triM %>% filter(facility.state %in% common_facility.state)
gc()
#======================================================================================================================#
### Keeping only common chemical across years---Panelize the chemicals
#======================================================================================================================#
gc()
# Split the chemical name column into a list of vectors by year
chemical_by_year <- split(triM$chemical.name, triM$year)

# Find the common chemicals across all years
common_chemical.name <- Reduce(f = intersect, x = chemical_by_year)

# Output the common chemicals
print(sort(common_chemical.name))

# Keep only common chemicals across years in the dataframe
triM <- triM %>% filter(chemical.name %in% common_chemical.name)
gc()
#======================================================================================================================#
### Keeping only common facility.id across years---Panelize the facility.ids
#======================================================================================================================#
# Split the facility id column into a list of vectors by year
# facility.id_by_year <- split(triM$facility.id, triM$year)
#
# # Find the common facility ids across all states
# common_facility.id <- Reduce(f = intersect, x = facility.id_by_year)
#
# # Output the common facility ids
# print(common_facility.id)
#
# # Keep only common facility ids across years in the dataframe
# triM <- triM %>% filter(facility.id %in% common_facility.id)
n_distinct(triM$facility.id)
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
    by = c("year" = "year", "fips_code" = "fips_code", "facility.id" = "facility.frs.id")
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
#   rename(zip_code = zipba, facility.name = name) %>%
#   select(c(facility.name, zip_code, sic, assets, year)) %>%
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

sum(is.na(triM$qcew))
n_distinct(triM$fips_code)
n_distinct(qcew$fips_code)
#======================================================================================================================#
### Merging TRI, BEA and QCEW Data
#======================================================================================================================#
bea <- read_rds(file = "./Data_PhD/US/BEA/bea.rds") %>% filter(year >= 2011 & year <= 2017)
# Remove last word in county labels
bea$county_name <- sub(pattern = "\\s+\\w+$", replacement = "", bea$county_name)
n_distinct(bea$fips_code)


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
    by = c("fips_code" = "fips_code", "facility.county" = "county_name", "facility.state" = "county_state",
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

sort(unique(triM$facility.state))
sum(is.na(triM$fips_code))
n_distinct(triQ$naics.code)
n_distinct(triQ$facility.id)
n_distinct(triQ$chemical.id)
#======================================================================================================================#
### Merging NBER-CES DATA---Manufacturing Industry Database
### Based on NAICS 2012
#======================================================================================================================#
gc()
start_time <- Sys.time()
triQ_manu <- triQ %>%
  filter(industry.category == "Manufacturing") %>%
  left_join(
    # y = read_csv(file = "./Data_PhD/US/NBER/nberces5818v1_n1997.csv") %>%
    y = read_rds(file = "./Data_PhD/US/NBER/nber_ces.rds") %>%
      filter(year >= 2011 & year <= 2017) %>%
      mutate(naics.code = as.character(naics)) %>%
      select(c(naics.code, year:plant, tfp4, tfp5)) %>%
      data.frame(),
    by = c("year" = "year", "naics.code" = "naics.code")
  ) %>%
  data.frame()
end_time <- Sys.time()
end_time - start_time
gc()

n_distinct(triQ_manu$naics.code)
n_distinct(triQ_manu$facility.id)
n_distinct(triQ_manu$chemical.id)
n_distinct(triQ_manu$facility.state)
sort(unique(triQ_manu$facility.state))
glimpse(triQ_manu)
triQ_manu <- triQ_manu %>%
  mutate(
    facility.latitude = as.character(facility.latitude),
    facility.longitude = as.character(facility.longitude),
    industrial.kiln.onsite = as.character(industrial.kiln.onsite),
    industrial.furnace.onsite = as.character(industrial.furnace.onsite),
    industrial.boiler.onsite = as.character(industrial.boiler.onsite),
    metal.recovery.onsite = as.character(metal.recovery.onsite),
    solvent.recovery.onsite = as.character(solvent.recovery.onsite),
    reuse.onsite = as.character(reuse.onsite),
    biological.treatment.onsite = as.character(biological.treatment.onsite),
    chemical.treatment.onsite = as.character(chemical.treatment.onsite),
    incineration.thermal.treatment.onsite = as.character(incineration.thermal.treatment.onsite),
    physical.treatment.onsite = as.character(physical.treatment.onsite),
    material.subandmod = as.character(material.subandmod),
    sub.fuel.matsubmod = as.character(sub.fuel.matsubmod),
    sub.organic.solvent.matsubmod = as.character(sub.organic.solvent.matsubmod),
    sub.rawm.feedstock.reactchem.matsubmod = as.character(sub.rawm.feedstock.reactchem.matsubmod),
    sub.manu.proccess.ancilliary.chems.matsubmod = as.character(sub.manu.proccess.ancilliary.chems.matsubmod),
    mod.content.grade.purity.chems.matsubmod = as.character(mod.content.grade.purity.chems.matsubmod),
    other.matmods.matsubmod = as.character(other.matmods.matsubmod),
    product.modification = as.character(product.modification),
    devd.newproductline.pmod = as.character(devd.newproductline.pmod),
    alt.dim.comp.design.pmod = as.character(alt.dim.comp.design.pmod),
    mod.packaging.pmod = as.character(mod.packaging.pmod),
    other.pmods.pmod = as.character(other.pmods.pmod),
    process.equip.modification = as.character(process.equip.modification),
    optimised.process.efficiency.pequipmod = as.character(optimised.process.efficiency.pequipmod),
    recirculationinprocess.pequipmod = as.character(recirculationinprocess.pequipmod),
    newtech.technique.process.pequipmod = as.character(newtech.technique.process.pequipmod),
    equipment.upgrade.update.pequipmod = as.character(equipment.upgrade.update.pequipmod),
    other.pequipmods.pequipmod = as.character(other.pequipmods.pequipmod),
    inventory.material.mgt = as.character(inventory.material.mgt),
    better.labelling.testing.immgt = as.character(better.labelling.testing.immgt),
    containers.sizechange.immgt = as.character(containers.sizechange.immgt),
    improved.materialhandling.operations.immgt = as.character(improved.materialhandling.operations.immgt),
    improved.monitoring.immgt = as.character(improved.monitoring.immgt),
    other.immgts.immgt = as.character(other.immgts.immgt),
    operating.practices.training = as.character(operating.practices.training),
    improved.schdule.operation.procedures.opt = as.character(improved.schdule.operation.procedures.opt),
    changed.production.schedule.opt = as.character(changed.production.schedule.opt),
    intro.inline.productquality.process.analysis.opt = as.character(intro.inline.productquality.process.analysis.opt),
    trade.secret = as.character(trade.secret),
    sanitised = as.character(sanitised),
    entire.facility = as.character(entire.facility),
    federal.facility = as.character(federal.facility),
    govt.owned.facility = as.character(govt.owned.facility),
    elemental.metal.included = as.character(elemental.metal.included),
    clean.air.act.chems = as.character(clean.air.act.chems),
    carcinogenic.chems = as.character(carcinogenic.chems),
    pfas.chems = as.character(pfas.chems),
    metal.restrict.tri = as.character(metal.restrict.tri),
    produced.chem.facility = as.character(produced.chem.facility),
    imported.chem.facility = as.character(imported.chem.facility),
    pi.chem.facility = as.character(pi.chem.facility),
    chemical.intermediate.uses = as.character(chemical.intermediate.uses),
    chemical.formulation.component = as.character(chemical.formulation.component),
    chemical.article.component = as.character(chemical.article.component),
    chemical.manufacturing.aid = as.character(chemical.manufacturing.aid),
    chemical.ancilliary.use = as.character(chemical.ancilliary.use),
    zip.length = as.character(zip.length),
    offsite.zip.length = as.character(offsite.zip.length),
    potw.zip.length = as.character(potw.zip.length),
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
n_distinct(fac_county_df$fips_code)
triQ <- triQ_manu %>%
  right_join(
    y = fac_county_df,
    by = c("fips_code" = "fips_code")
  )
glimpse(triQ)
triQ_na <- triQ[is.na(triQ$facility.zipcode),]
triQ <- triQ[complete.cases(triQ$facility.zipcode),]
sum(is.na(triQ))
sum(is.na(triQ %>% select(-c(contains(match = "offsite"), contains(match = "potw")))))
sum(is.na(triQ$fips_code))
sum(is.na(triQ$facility.zipcode))
sum(is.na(triQ$facility.city))
n_distinct(triQ$fips_code)
n_distinct(triQ$facility.id)
n_distinct(triQ$state.code)
n_distinct(triQ$facility.state)
sort(unique(triQ$state))
glimpse(triQ)

gc()
start_time <- Sys.time()
write_rds(x = triQ, file = "./Data_PhD/US/BLS/triQ.rds", compress = "xz")
end_time <- Sys.time()
end_time - start_time
gc()
sum_up(triQ, c(total.fug.air.emissions.onsite, total.point.air.emissions.onsite, total.air.emissions.onsite))
gc()
#======================================================================================================================#