#======================================================================================================================#
### Packages
#======================================================================================================================#
library(tidyverse)
library(usgeogr)
#======================================================================================================================#
### Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/Davidmac.Ekeocha/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading QCEW Data
#======================================================================================================================#
filepath2011 <- "./Data_PhD/US/EPA/AQS/ghg/ghgp2011.csv"
filepath2012 <- "./Data_PhD/US/EPA/AQS/ghg/ghgp2012.csv"
filepath2013 <- "./Data_PhD/US/EPA/AQS/ghg/ghgp2013.csv"
filepath2014 <- "./Data_PhD/US/EPA/AQS/ghg/ghgp2014.csv"
filepath2015 <- "./Data_PhD/US/EPA/AQS/ghg/ghgp2015.csv"
filepath2016 <- "./Data_PhD/US/EPA/AQS/ghg/ghgp2016.csv"
filepath2017 <- "./Data_PhD/US/EPA/AQS/ghg/ghgp2017.csv"

ghgp11 <- read.csv(file = filepath2011, header = T) %>%
  mutate(year = 2011) %>%
  lapply(., as.character)
ghgp12 <- read.csv(file = filepath2012, header = T) %>%
  mutate(year = 2012) %>%
  lapply(., as.character)
ghgp13 <- read.csv(file = filepath2013, header = T) %>%
  mutate(year = 2013) %>%
  lapply(., as.character)
ghgp14 <- read.csv(file = filepath2014, header = T) %>%
  mutate(year = 2014) %>%
  lapply(., as.character)
ghgp15 <- read.csv(file = filepath2015, header = T) %>%
  mutate(year = 2015) %>%
  lapply(., as.character)
ghgp16 <- read.csv(file = filepath2016, header = T) %>%
  mutate(year = 2016) %>%
  lapply(., as.character)
ghgp17 <- read.csv(file = filepath2017, header = T) %>%
  mutate(year = 2017) %>%
  lapply(., as.character)

ghgp <- bind_rows(ghgp11, ghgp12, ghgp13, ghgp14, ghgp15, ghgp16, ghgp17) %>% data.frame()

ghgp <- ghgp %>%
  rename(
    facility.id = Facility.Id,
    facility.frs.id = FRS.Id,
    facility.city = City,
    facility.state.code = State,
    facility.zipcode = Zip.Code,
    facility.county = County,
    naics.code = Primary.NAICS.Code,
    co2.emissions = CO2.emissions..non.biogenic.,
    methane.emissions = Methane..CH4..emissions,
    nitrousoxide.emissions = Nitrous.Oxide..N2O..emissions,
  ) %>%
  select(c(year, facility.id, facility.frs.id, facility.zipcode, facility.city, facility.county,
           facility.state.code, naics.code, co2.emissions, methane.emissions, nitrousoxide.emissions)) %>%
  data.frame()


# Making first letters uppercase
ghgp$facility.county <- tolower(ghgp$facility.county)  # Convert entire column to lowercase
ghgp$facility.county <- gsub(pattern = "(^|\\s)([a-z])", replacement = "\\1\\U\\2", ghgp$facility.county, perl = T)  # Capitalize first letter of each word

ghgp$facility.city <- tolower(ghgp$facility.city)  # Convert entire column to lowercase
ghgp$facility.city <- gsub(pattern = "(^|\\s)([a-z])", replacement = "\\1\\U\\2", ghgp$facility.city, perl = T)  # Capitalize first letter of each word
write_rds(ghgp, file = "./Data_PhD/US/EPA/AQS/ghg/ghgp.rds", compress = "xz")
ghgp <- read_rds(file = "./Data_PhD/US/EPA/AQS/ghg/ghgp.rds")

#======================================================================================================================#
### HAPS
#======================================================================================================================#
filepath <- "C:/Users/Davidmac.Ekeocha/Downloads/Compressed/2017_ama_haps/2017_AMA_HAPS.txt"

data <- data.table::fread(file = filepath, header = T)