####################
# 20 September 2023
####################
# https://shorturl.at/uBCDX         # for loading json strings
# library(data.table)
# library(eiastats) #https://rdrr.io/github/mitcda/eiastats/
# library(stringr)
# library(rjson)
library(tidyverse)
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("reshape2")
# remotes::install_github("mitcda/eiastats")
# library(httr)
# library(jsonlite)
library(reshape2)
library(ndjson)


# ####################################################################################################
# ### EIA - EMISSIONS - ANNUAL -  bulk download
# ####################################################################################################
filepath <- "C:/Users/david/OneDrive/Documents/ULMS/PhD/Data_PhD/US/EIA/EMISS/EMISS.txt"

emissions <- tibble::as_tibble(ndjson::stream_in(filepath))
glimpse(emissions)
dim(emissions)

# emissions <- emissions %>% unnest(.)
# glimpse(emissions)

emissions <- select(emissions, c(series_id, geoset_id, iso3166, geography, name, description, source,
								 start, end, f, units, unitsshort, last_updated, starts_with(match = "data")))
unique(emissions$name)

# Residential carbon dioxide emissions, all fuels
res.emiss.all <- emissions %>% filter(geoset_id == "EMISS.CO2-TOTV-RC-TO.A")

res.emiss.all1 <- res.emiss.all %>%
  pivot_longer(cols = ends_with("1"), names_to = "d1", values_to = "residential.emissions.allfuels") %>%
  select(!ends_with("0"))

res.emiss.all2 <- res.emiss.all %>%
  pivot_longer(cols = ends_with("0"), names_to = "d0", values_to = "year") %>%
  select(c(d0, year))
res.emiss.allfuels <- cbind(res.emiss.all1, res.emiss.all2) %>% na.omit()

# Residential carbon dioxide emissions, residential
res.emiss.resid <- emissions %>%
  filter(geoset_id == "EMISS.CO2-V-CLRCB.A") %>%
  select(!c(starts_with("data.39"):starts_with("data.51"), data.51.1))

res.emiss.resid1 <- res.emiss.resid %>%
  pivot_longer(cols = ends_with("1"), names_to = "d1", values_to = "residential.emissions.residential") %>%
  select(!ends_with("0"))

res.emiss.resid2 <- res.emiss.resid %>%
  pivot_longer(cols = ends_with("0"), names_to = "d0", values_to = "year") %>%
  select(c(d0, year))
res.emiss.residential <- cbind(res.emiss.resid1, res.emiss.resid2) %>% na.omit()

# Residential carbon dioxide emissions, petroleum
res.emiss.petrol <- emissions %>% filter(geoset_id == "EMISS.CO2-TOTV-RC-PE.A")

res.emiss.petrol1 <- res.emiss.petrol %>%
  pivot_longer(cols = ends_with("1"), names_to = "d1", values_to = "residential.emissions.petrol") %>%
  select(!ends_with("0"))

res.emiss.petrol2 <- res.emiss.petrol %>%
  pivot_longer(cols = ends_with("0"), names_to = "d0", values_to = "year") %>%
  select(c(d0, year))
res.emiss.petroleum <- cbind(res.emiss.petrol1, res.emiss.petrol2) %>% na.omit()

# Residential carbon dioxide emissions, natural gas
res.emiss.ngas <- emissions %>% filter(geoset_id == "EMISS.CO2-TOTV-RC-NG.A")

res.emiss.ngas1 <- res.emiss.ngas %>%
  pivot_longer(cols = ends_with("1"), names_to = "d1", values_to = "residential.emissions.ngas") %>%
  select(!ends_with("0"))

res.emiss.ngas2 <- res.emiss.ngas %>%
  pivot_longer(cols = ends_with("0"), names_to = "d0", values_to = "year") %>%
  select(c(d0, year))
res.emiss.naturalgas <- cbind(res.emiss.ngas1, res.emiss.ngas2) %>% na.omit()

# Residential carbon dioxide emissions, coal
res.emiss.coal <- emissions %>% filter(geoset_id == "EMISS.CO2-TOTV-RC-CO.A")

res.emiss.coal1 <- res.emiss.coal %>%
  pivot_longer(cols = ends_with("1"), names_to = "d1", values_to = "residential.emissions.coal") %>%
  select(!ends_with("0"))

res.emiss.coal2 <- res.emiss.coal %>%
  pivot_longer(cols = ends_with("0"), names_to = "d0", values_to = "year") %>%
  select(c(d0, year))
res.emissions.coal <- cbind(res.emiss.coal1, res.emiss.coal2) %>% na.omit()

# Residential carbon dioxide emissions, natural gas (pipeline)
res.emiss.ngas.pipe <- emissions %>% filter(geoset_id == "EMISS.CO2-V-NGRCB.A")

res.emiss.ngas.pipe1 <- res.emiss.ngas.pipe %>%
  pivot_longer(cols = ends_with("1"), names_to = "d1", values_to = "residential.emissions.ngas(pipeline)") %>%
  select(!ends_with("0"))

res.emiss.ngas.pipe2 <- res.emiss.ngas.pipe %>%
  pivot_longer(cols = ends_with("0"), names_to = "d0", values_to = "year") %>%
  select(c(d0, year))
res.emiss.ngas.pipeline <- cbind(res.emiss.ngas.pipe1, res.emiss.ngas.pipe2) %>% na.omit()

# Residential carbon dioxide emissions, LPG(fuel use)
res.emiss.lpg <- emissions %>%
  filter(geoset_id == "EMISS.CO2-V-HLRCB.A") %>%
  select(!c(starts_with("data.39"):starts_with("data.51"), data.51.1))

res.emiss.lpg1 <- res.emiss.lpg %>%
  pivot_longer(cols = ends_with("1"), names_to = "d1", values_to = "residential.emissions.lpg(fueluse)") %>%
  select(!ends_with("0"))

res.emiss.lpg2 <- res.emiss.lpg %>%
  pivot_longer(cols = ends_with("0"), names_to = "d0", values_to = "year") %>%
  select(c(d0, year))
res.emiss.lpguse <- cbind(res.emiss.lpg1, res.emiss.lpg2) %>% na.omit()

# Residential carbon dioxide emissions, Distillate Fuel Use
res.emiss.distillate <- emissions %>%
  filter(geoset_id == "EMISS.CO2-V-DFRCB.A") %>%
  select(!c(starts_with("data.39"):starts_with("data.51"), data.51.1))

res.emiss.dist1 <- res.emiss.distillate %>%
  pivot_longer(cols = ends_with("1"), names_to = "d1", values_to = "residential.emissions.distillate(fueluse)") %>%
  select(!ends_with("0"))

res.emiss.dist2 <- res.emiss.distillate %>%
  pivot_longer(cols = ends_with("0"), names_to = "d0", values_to = "year") %>%
  select(c(d0, year))
res.emiss.distillateuse <- cbind(res.emiss.dist1, res.emiss.dist2) %>% na.omit()

# Residential carbon dioxide emissions, Kerosene
res.emiss.kero <- emissions %>%
  filter(geoset_id == "EMISS.CO2-V-KSRCB.A") %>%
  select(!c(starts_with("data.39"):starts_with("data.51"), data.51.1))

res.emiss.kero1 <- res.emiss.kero %>%
  pivot_longer(cols = ends_with("1"), names_to = "d1", values_to = "residential.emissions.kerosene(fueluse)") %>%
  select(!ends_with("0"))

res.emiss.kero2 <- res.emiss.kero %>%
  pivot_longer(cols = ends_with("0"), names_to = "d0", values_to = "year") %>%
  select(c(d0, year))
res.emiss.kerosene <- cbind(res.emiss.kero1, res.emiss.kero2) %>% na.omit()


#############################################################################
### EIA - Cost and Savings from Energy Efficiency Programmes 2013-2021
#############################################################################
### URLs
base_url <- "https://api.eia.gov/v2/"
api_key <- "&api_key=Ls5Dzb9Qf8g5jmnpdgcM5S6hqDKGvM7KdIAuiwAK"

cost_savings_from_ee_programs_api_url <- "electricity/state-electricity-profiles/energy-efficiency/data/?frequency=annual&data[0]=all-other-costs&data[1]=customer-incentive&data[2]=energy-savings&data[3]=potential-peak-savings&facets[state][]=AK&facets[state][]=AL&facets[state][]=AR&facets[state][]=AZ&facets[state][]=CA&facets[state][]=CO&facets[state][]=CT&facets[state][]=DC&facets[state][]=DE&facets[state][]=FL&facets[state][]=GA&facets[state][]=HI&facets[state][]=IA&facets[state][]=ID&facets[state][]=IL&facets[state][]=IN&facets[state][]=KS&facets[state][]=KY&facets[state][]=LA&facets[state][]=MA&facets[state][]=MD&facets[state][]=ME&facets[state][]=MI&facets[state][]=MN&facets[state][]=MO&facets[state][]=MS&facets[state][]=MT&facets[state][]=NC&facets[state][]=ND&facets[state][]=NE&facets[state][]=NH&facets[state][]=NJ&facets[state][]=NM&facets[state][]=NV&facets[state][]=NY&facets[state][]=OH&facets[state][]=OK&facets[state][]=OR&facets[state][]=PA&facets[state][]=RI&facets[state][]=SC&facets[state][]=SD&facets[state][]=TN&facets[state][]=TX&facets[state][]=US&facets[state][]=UT&facets[state][]=VA&facets[state][]=VT&facets[state][]=WA&facets[state][]=WI&facets[state][]=WV&facets[state][]=WY&start=2013&end=2021&sort[0][column]=period&sort[0][direction]=asc&sort[1][column]=state&sort[1][direction]=asc&sort[2][column]=sector&sort[2][direction]=asc&offset=0&length=5000"

full_api_url <- base::paste0(base_url, cost_savings_from_ee_programs_api_url, api_key)

### API CALLS
cost.savings.ee <- httr::GET(full_api_url)
cost.savings.ee$status_code
cost.savings.ee$content

### Convert to readable data
cost.savings.ee.api.char <- base::rawToChar(cost.savings.ee$content) # converts to characters
cost.savings.ee.api.json <- jsonlite::fromJSON(cost.savings.ee.api.char, flatten = TRUE)
cost.savings.ee.api.json.20132021.df <- cost.savings.ee.api.json$response$data


# ####################################################################################################
# ### EIA - ELECTRICITY - f = A, Q, M -  bulk download
# ####################################################################################################
filepath <- "C:/Users/david/OneDrive/Documents/ULMS/PhD/Data_PhD/US/EIA/ELEC/ELEC.txt"
elec <- tibble::as_tibble(ndjson::stream_in(filepath))
glimpse(elec)
dim(elec)

# elec <- elec %>% unnest(.)
# glimpse(elec)

elec <- select(elec, c(series_id, geoset_id, iso3166, geography, name, description, source,
					   start, end, f, units, last_updated, starts_with(match = "data")))
unique(elec$name)
unique(elec$f)

elec.annual <- elec %>% filter(f == "A")
elec.annual.ak <- elec.annual %>% filter(iso3166 == "USA-AK")
glimpse(elec.annual)
unique(elec.annual$name)

# Residential Sector
res.elec.price <- elec.annual %>% filter(geoset_id == "ELEC.PRICE.RES.A")
# elec.price <- elec.annual %>% filter(series_id == "ELEC.PRICE.AL-RES.A")
res.elec.price <- select(res.elec.price,
						 c(geography, name, description, source, units),
						 # years
						 c(data.21.0, data.20.0, data.19.0, data.18.0, data.17.0, data.16.0, data.15.0, data.14.0,
						   data.13.0, data.12.0, data.11.0, data.10.0, data.9.0, data.8.0, data.7.0, data.6.0,
						   data.5.0, data.4.0, data.3.0, data.2.0, data.1.0, data.0.0),
						 # values
						 c(data.21.1, data.20.1, data.19.1, data.18.1, data.17.1, data.16.1, data.15.1, data.14.1,
						   data.13.1, data.12.1, data.11.1, data.10.1, data.9.1, data.8.1, data.7.1, data.6.1,
						   data.5.1, data.4.1, data.3.1, data.2.1, data.1.1, data.0.1))

res.elec.price <- res.elec.price %>% mutate(across(c(data.21.0, data.20.0, data.19.0, data.18.0, data.17.0, data.16.0,
													 data.15.0, data.14.0, data.13.0, data.12.0, data.11.0, data.10.0,
													 data.9.0, data.8.0, data.7.0, data.6.0, data.5.0, data.4.0,
													 data.3.0, data.2.0, data.1.0, data.0.0), as.numeric))

write_rds(res.elec.price, file = "src/energy/data/test.rds", compress = "xz")
# res.elec.price <- t(res.elec.price) %>% data.frame()

res.elec.price1 <- res.elec.price %>% pivot_longer(cols = ends_with("0"), names_to = "d0", values_to = "year")
res.elec.price1 <- res.elec.price1 %>% select(c(d0, year))
res.elec.price2 <- res.elec.price %>% pivot_longer(cols = ends_with("1"), names_to = "d1", values_to = "retail.price")
res.elec.price2 <- res.elec.price2 %>% select(!ends_with("0"))
res.elec.price2 <- cbind(res.elec.price2, res.elec.price1)

# ####################################################################################################
# ### EIA - STATE ENERGY DATA SYSTEM (SEDS)- ANNUAL AND MONTHLY - bulk download
# ####################################################################################################
filepath <- "C:/Users/david/OneDrive/Documents/ULMS/PhD/Data_PhD/US/EIA/SEDS/SEDS.txt"

# Stream in the data and unnest
seds <- tibble::as_tibble(ndjson::stream_in(filepath))
glimpse(seds)
dim(seds)

# seds.unnest <- seds %>% unnest(.)
# glimpse(seds.unnest)
seds <- select(seds, c(series_id, geoset_id, iso3166, geography, name, description, source,
					   start, end, f, units, last_updated, starts_with(match = "data")))
seds.ak <- seds %>% filter(iso3166 == "USA-AK")

# ####################################################################################################
# ### EIA - ELECTRIC SYSTEM OPERATING DATA (EBA) - DWMA - bulk download
# ####################################################################################################
filepath <- "C:/Users/david/OneDrive/Documents/ULMS/PhD/Data_PhD/US/EIA/EBA/EBA.txt"

# Stream in the data and unnest
# eba <- tbl_df(ndjson::stream_in(filepath))
eba <- tibble::as_tibble(ndjson::stream_in(filepath))
glimpse(eba)
dim(eba)
unique(eba$f)
eba.annual <- eba %>% filter(f == "A")
glimpse(eba.annual)

# seds.unnest <- seds %>% unnest(.)
# glimpse(seds.unnest)
eba.annual <- select(eba.annual, c(series_id, geoset_id, name, description, start, end, f, units, last_updated,
					 starts_with(match = "data")))
eba.annual.DH <- eba.annual %>% filter(geoset_id == "EBA.D.H")

#####################################################################################################
#### EIA - NATURAL GAS (NG) - AMQ - bulk download
#####################################################################################################
filepath <- "C:/Users/david/OneDrive/Documents/ULMS/PhD/Data_PhD/US/EIA/NG/NG.txt"

# Stream in the data and unnest
# eba <- tbl_df(ndjson::stream_in(filepath))
ng <- tibble::as_tibble(ndjson::stream_in(filepath))
glimpse(ng)
dim(ng)

# seds.unnest <- seds %>% unnest(.)
# glimpse(seds.unnest)
unique(ng$f)

ng.annual <- ng %>% filter(f == "A")

ng.annual <- select(ng.annual, c(series_id, iso3166, geography, name, description, source,
					   start, end, f, units, last_updated, starts_with(match = "data")))

ng.annual.ak <- ng.annual %>% filter(iso3166 == "USA-AK")

####################################################################
### ACS data
####################################################################
# DP02 - Social
base.acs.url <- "https://api.census.gov/"
dp.ap.url <- "data/2006/acs/acs1/profile?get=group(DP02)&for=county:*&in=state:*"
full.api.url <- base::paste0(base.acs.url, dp.ap.url)

acs2022.dp02.social <- httr::GET(full.api.url)
acs2022.dp02.social$status_code
acs2022.dp02.social$content

acs2022.dp02.social.char <- base::rawToChar(acs2022.dp02.social$content)
acs2022.dp02.social.json <- jsonlite::fromJSON(acs2022.dp02.social.char, flatten = T)
# acs2005.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
acs2006.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2007.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2008.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2009.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2010.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2011.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2012.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2013.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2014.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2015.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2016.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2017.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2018.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2019.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2021.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
# acs2022.dp02.social.df <- as.data.frame(acs2022.dp02.social.json)
str(acs2022.dp02.social)

acs2005.dp02.social.df <- acs2005.dp02.social.df %>% mutate(year = 2005)
write.csv(acs2005.dp02.social.df, file = "src/energy/data/acs2005.dp02.social.csv")
acs2005.dp02.social.df <- read.csv(file = "src/energy/data/acs2005.dp02.social.csv")
acs2005.dp02.social.df <- select(acs2005.dp02.social.df, !c(ends_with("EA"), ends_with("MA"), ends_with("M")))
write.csv(acs2005.dp02.social.df, file = "src/energy/data/acs2005.dp02.social.csv")
acs2005.dp02.social.df <- read.csv(file = "src/energy/data/acs2005.dp02.social.csv")
str(acs2005.dp02.social.df)

# acs2006.dp02.social.df <- acs2006.dp02.social.df %>% mutate(year = 2006)
# write.csv(acs2006.dp02.social.df, file = "src/energy/data/acs2006.dp02.social.csv") #removed undesired column
# characters
# acs2006.dp02.social.df <- read.csv(file = "src/energy/data/acs2006.dp02.social.csv")
# acs2006.dp02.social.df <- select(acs2006.dp02.social.df, !c(ends_with("EA"),  ends_with("MA"),  ends_with("M")))
# write.csv(acs2006.dp02.social.df, file = "src/energy/data/acs2006.dp02.social.csv")
# acs2006.dp02.social.df <- read.csv(file = "src/energy/data/acs2006.dp02.social.csv")
# str(acs2006.dp02.social.df)
acs2007.dp02.social.df <- acs2007.dp02.social.df %>% mutate(year = 2007)
acs2008.dp02.social.df <- acs2008.dp02.social.df %>% mutate(year = 2008)
acs2009.dp02.social.df <- acs2009.dp02.social.df %>% mutate(year = 2009)
acs2010.dp02.social.df <- acs2010.dp02.social.df %>% mutate(year = 2010)
acs2011.dp02.social.df <- acs2011.dp02.social.df %>% mutate(year = 2011)
acs2012.dp02.social.df <- acs2012.dp02.social.df %>% mutate(year = 2012)
acs2013.dp02.social.df <- acs2013.dp02.social.df %>% mutate(year = 2013)
acs2014.dp02.social.df <- acs2014.dp02.social.df %>% mutate(year = 2014)
acs2015.dp02.social.df <- acs2015.dp02.social.df %>% mutate(year = 2015)
acs2016.dp02.social.df <- acs2016.dp02.social.df %>% mutate(year = 2016)
acs2017.dp02.social.df <- acs2017.dp02.social.df %>% mutate(year = 2017)
acs2018.dp02.social.df <- acs2018.dp02.social.df %>% mutate(year = 2018)
acs2019.dp02.social.df <- acs2019.dp02.social.df %>% mutate(year = 2019)
acs2021.dp02.social.df <- acs2021.dp02.social.df %>% mutate(year = 2021)
acs.dp02.social <- bind_rows(
  acs2005.dp02.social.df,
  acs2006.dp02.social.df,
  acs2007.dp02.social.df,
  acs2008.dp02.social.df,
  acs2009.dp02.social.df,
  acs2010.dp02.social.df,
  acs2011.dp02.social.df,
  acs2012.dp02.social.df,
  acs2013.dp02.social.df,
  acs2014.dp02.social.df,
  acs2015.dp02.social.df,
  acs2016.dp02.social.df,
  acs2017.dp02.social.df,
  acs2018.dp02.social.df,
  acs2019.dp02.social.df,
  acs2021.dp02.social.df,
  .id = "cid"
)

write.csv(acs2005.dp02.social.df, file = "src/energy/data/acs2005.dp02.social.csv")
acs2005.dp02.social.df <- read.csv(file = "src/energy/data/acs2005.dp02.social.csv")
