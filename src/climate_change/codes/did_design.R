#======================================================================================================================#
### PhD Chapter 3
### Indirect Consequences of a Raising Minimum Wage
### 30 November 2023
### Using Border-County Designs
#======================================================================================================================#
### Packages
#======================================================================================================================#
library(tidyverse)
library(statar)
library(usgeogr)
# install.packages("remotes")
# remotes::install_github("davidsovich/usgeogr")
library(usmap)
#======================================================================================================================#
### Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading Data
#======================================================================================================================#
gc()
start_time <- Sys.time()
triQ.manu <- read_rds(file = "./Data_PhD/US/BLS/triQ.manu.rds") %>%
  group_by(facility.id, year) %>%
  select(
	c(
	  year, facility.id, facility.zipcode, facility.city, facility.county, facility.state.code, facility.state,
	  facility.latitude, facility.longitude, offsite.id, offsite.facility.id, offsite.zipcode, offsite.city,
	  offsite.county, offsite.state, offsite.countryid, potw.id, potw.zipcode, potw.city, potw.county, potw.state,
	  fips_code, neighbor_state, neighbor_fips_code, neighbor_lat, neighbor_long,
	  relaxed_cpcp_id:county_dist_to_segment, naics.code:chemical.name, mixture,
	  chemical.classification:chemical.ancilliary.use, population, neighbor_population, personal_income:vadd,
	  invent, energy
	)
  ) %>%
  data.frame()
triQ.manu <- triQ.manu[complete.cases(triQ.manu$total_annual_wages),]
end_time <- Sys.time()
end_time - start_time
gc()
#======================================================================================================================#
### Check if dataframe is a panel data
#======================================================================================================================#
is_panel <- function(df) {
  # Check if there are at least two columns representing different time periods
  if (ncol(df) < 2) {
	return(FALSE)
  }

  # Check if there are at least two rows representing different individuals
  if (nrow(df) < 2) {
	return(FALSE)
  }

  return(TRUE)
}

# Example usage:
# Assuming df is your dataframe
if (is_panel(df = triQ.manu)) {
  print("The dataframe resembles panel data.")
} else {
  print("The dataframe does not resemble panel data.")
}

#======================================================================================================================#
### Converting IDs to numeric
### Experiment Design---TWFE: California raised MW >= $1 in 2015
#======================================================================================================================#
triQ.manu <- triQ.manu %>%
  mutate(
	facility.id = as.numeric(facility.id),
	facility.zipcode = as.numeric(facility.zipcode),
	fips_code = as.numeric(fips_code),
	facility.state.id = as.numeric(as.factor(facility.state)),
	naics.code = as.numeric(naics.code),
  )

triQ.manu[162:164] <- lapply(triQ.manu[162:164], as.numeric)
triQ.manu[166] <- lapply(triQ.manu[166], as.numeric)
triQ.manu[168:181] <- lapply(triQ.manu[168:181], as.numeric)
#======================================================================================================================#
### Getting air emissions chemicals
#======================================================================================================================#
sort(unique(triQ.manu$chemical.name))
sort(unique(triQ.manu[triQ.manu$chemical.classification == "TRI",]$chemical.name))
sort(unique(triQ.manu[triQ.manu$chemical.classification == "PBT",]$chemical.name))
sort(unique(triQ.manu[triQ.manu$chemical.classification == "DIOXIN",]$chemical.name))
table(triQ.manu$chemical.classification)
#======================================================================================================================#
### TRI for researchers: source - https://shorturl.at/kqvy7
### CAUSING CANCER: source- https://ntp.niehs.nih.gov/whatwestudy/assessments/cancer/roc#toc1
#======================================================================================================================#
# Outdoor air pollutants
# Arsenic, arsenic compounds,Chromium, Chromium compounds (except for chromite ore mined in the Transvaal Region),
# Nickel, Nickel compounds, Vanadium (except when contained in an alloy), Vanadium compounds.

# Indoor air pollutants
# Asbestos (friable),

# Both indoor and outdoor air pollutants
# Lead, Lead compounds, Mercury, Mercury compounds, Ammonia,
# hazardous air pollutants (benzene, 1,3-Butadiene, Hexachloro-1,3-butadiene)
#======================================================================================================================#
### Experimental Design---matching counties in treated states to adjacent border counties in the control states
### Onsite
gc()
#======================================================================================================================#
sort(unique(triQ.manu$facility.state))

# Selecting the treated and control states
triQ.on <- triQ.manu %>%
  mutate(
	# Year of first MW raise >= $0.5
	mw.year = case_when(
	  facility.state == "Arkansas" ~ 2015,
	  facility.state == "California" ~ 2014,
	  facility.state == "Delaware" ~ 2014,
	  facility.state == "Maryland" ~ 2015,
	  facility.state == "Michigan" ~ 2014,
	  facility.state == "Minnesota" ~ 2014,
	  facility.state == "Nebraska" ~ 2015,
	  facility.state == "New York" ~ 2014,
	  facility.state == "West Virginia" ~ 2015, T ~ Inf
	),
	mw.raise = case_when(
	  facility.state %in%
		c("Arkansas", "California", "Delaware", "Maryland", "Michigan", "Minnesota",
		  "Nebraska", "New York", "West Virginia")
		~ 1, T ~ 0
	),
	post = case_when(year == 2014 | year == 2015 ~ 1, T ~ 0)
  ) %>%
  select(
	c(
	  year, facility.id, facility.zipcode, facility.city, facility.county, facility.state.code, facility.state,
	  facility.latitude, facility.longitude, fips_code, neighbor_state, neighbor_lat, neighbor_long,
	  relaxed_cpcp_id:chemical.name, chemical.classification, unit.of.measure, contains(match = "onsite"),
	  material.subandmod:intro.inline.productquality.process.analysis.opt, trade.secret:post
	)
  ) %>%
  data.frame()

sort(unique(triQ.on$facility.state))

# relative years
triQ.on <- triQ.on %>% mutate(rel_year = year - mw.year + 2014)
sort(unique(triQ.on$rel_year))
sort(unique(triQ.on$facility.state))

triQ.on %>%
  group_by(facility.state, neighbor_state) %>%
  filter(mw.year != Inf) %>%
  summarise(count = facility.county %>% n_distinct()) %>%
  arrange(desc(count)) %>%
  print(n = nrow(.))

triQ.on %>%
  group_by(facility.state, neighbor_state) %>%
  filter(mw.year == Inf) %>%
  summarise(count = facility.county %>% n_distinct()) %>%
  arrange(desc(count)) %>%
  print(n = nrow(.))

#======================================================================================================================#
# Selecting the cross-border treated and control states
gc()
#======================================================================================================================#
triQ.oncb <- triQ.manu %>%
  # Selecting only the states that share a border
  filter(
	facility.state %in%
	  c("Arkansas", "California", "Delaware", "Maryland", "Michigan", "Minnesota",
		"Nebraska", "New York", "West Virginia", "Oklahoma", "Texas", "Nevada",
		"Pennsylvania", "Virginia", "Wisconsin", "Indiana", "Illinois", "Iowa", "Kansas",
		"Wyoming", "Kentucky")
	  &
	  neighbor_state %in%
		c("AR", "OK", "TX", "CA", "NV", "DE", "MD", "PA", "MD", "VA", "WV", "MI",
		  "WI", "IN", "IL", "MN", "IA", "NE", "KS", "WY", "NY", "KY", "ND")
  ) %>%
  mutate(
	# Year of first MW raise >= $0.5
	mw.year = case_when(
	  facility.state == "Arkansas" ~ 2015,
	  facility.state == "California" ~ 2014,
	  facility.state == "Delaware" ~ 2014,
	  facility.state == "Maryland" ~ 2015,
	  facility.state == "Michigan" ~ 2014,
	  facility.state == "Minnesota" ~ 2014,
	  facility.state == "Nebraska" ~ 2015,
	  facility.state == "New York" ~ 2014,
	  facility.state == "West Virginia" ~ 2015, T ~ Inf
	),
	mw.raise = case_when(
	  facility.state %in%
		c("Arkansas", "California", "Delaware", "Maryland", "Michigan",
		  "Minnesota", "Nebraska", "New York", "West Virginia")
		~ 1, T ~ 0
	),
	post = case_when(year == 2014 | year == 2015 ~ 1, T ~ 0)
  ) %>%
  select(
	c(
	  year, facility.id, facility.zipcode, facility.city, facility.county, facility.state.code, facility.state,
	  facility.latitude, facility.longitude, fips_code, neighbor_state, neighbor_lat, neighbor_long,
	  relaxed_cpcp_id:chemical.name, chemical.classification, unit.of.measure, contains(match = "onsite"),
	  material.subandmod:intro.inline.productquality.process.analysis.opt, trade.secret:post
	)
  ) %>%
  data.frame()
#----------------------------------------------------------------------------------------------------------------------#
# Removing common counties in both treated and control states
#----------------------------------------------------------------------------------------------------------------------#
# Identify common counties between treated and control states
common_counties <- intersect(
  triQ.oncb$facility.county[triQ.oncb$mw.year == Inf],
  triQ.oncb$facility.county[triQ.oncb$mw.year != Inf]
)

# Remove common counties from the dataframe
triQ.oncb <- triQ.oncb[!triQ.oncb$facility.county %in% common_counties,]

sort(unique(triQ.oncb$facility.state))
n_distinct(triQ.oncb$facility.state)

table(triQ.oncb$year)
table(triQ.oncb$mw.year)
table(triQ.oncb$year, triQ.oncb$mw.year)

# Creating group G dummy: states treated by period g
triQ.oncb$treated <- as.numeric(triQ.oncb$year >= triQ.oncb$mw.year)

triQ.oncb %>%
  group_by(year) %>%
  summarise(treated = sum(treated))

sum_up(triQ.oncb %>% filter(treated == 1),
	   c(oty_total_annual_wages_chg, oty_total_annual_wages_pct_chg,
		 oty_avg_annual_pay_chg, oty_avg_annual_pay_pct_chg))

sum_up(triQ.oncb %>% filter(treated == 0),
	   c(oty_total_annual_wages_chg, oty_total_annual_wages_pct_chg,
		 oty_avg_annual_pay_chg, oty_avg_annual_pay_pct_chg))


triQ.oncb %>%
  group_by(facility.state) %>%
  filter(treated == 1) %>%
  summarise(county = facility.county %>% n_distinct()) %>%
  arrange(desc(county)) %>%
  print(., n = nrow(.))

triQ.oncb %>%
  group_by(facility.state, neighbor_state) %>%
  filter(treated == 0) %>%
  summarise(county = facility.county %>% n_distinct()) %>%
  arrange(desc(county)) %>%
  print(., n = nrow(.))

sum_up(triQ.oncb, emp:energy)

n_distinct(triQ.oncb$naics.code)
n_distinct(triQ.oncb$facility.id)
n_distinct(triQ.oncb$facility.county)
n_distinct(triQ.oncb[triQ.oncb$mw.year != Inf,]$facility.county)
n_distinct(triQ.oncb[triQ.oncb$mw.year == Inf,]$facility.county)
n_distinct(triQ.oncb$facility.state)
n_distinct(triQ.oncb[triQ.oncb$mw.year == Inf,]$facility.state)
n_distinct(triQ.oncb[triQ.oncb$mw.year != Inf,]$facility.state)
n_distinct(triQ.oncb$chemical.name)
n_distinct(triQ.oncb$relaxed_cpcp_id)
n_distinct(triQ.oncb$industry.name)
sort(unique(triQ.oncb$facility.state))
sort(unique(triQ.oncb$chemical.name))
sort(unique(triQ.oncb$chemical.classification))
sort(unique(triQ.oncb$industry.name))

sum_up(triQ.oncb, c(total.fug.air.emissions.onsite, total.point.air.emissions.onsite, total.air.emissions.onsite))

triQ.oncb %>%
  group_by(industry.name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  print(n = nrow(.))

triQ.oncb %>%
  group_by(chemical.name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  print(n = nrow(.))

triQ.oncb %>%
  group_by(facility.state) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  print(n = nrow(.))
#======================================================================================================================#
### Mapping the geographic units
#======================================================================================================================#
# triQ.map <- triQ.oncb %>%
#   select(facility.state, fips_code, facility.county, facility.latitude, facility.longitude) %>%
#   lapply(., as.character) %>%
#   data.frame()

library(maps)
library(mapproj)
library(ggplot2)

triQ.oncb <- triQ.oncb[order(triQ.oncb$state_border_id),]

ggplot(
  data = triQ.oncb,
  aes(x = facility.longitude, y = facility.latitude),
  color = "black"
) +
  geom_polygon(aes(group = facility.state)) +
  coord_map()

#======================================================================================================================#
### Offsite and Offsite
#======================================================================================================================#
# triQ.off <- triQ.manu %>%
#   select(
# 	c(
# 	  year, fips_code:unit.of.measure, contains(match = "offsite"), contains(match = "potw"), trade.secret:energy
# 	)) %>%
#   select(
# 	c(year, offsite.id:offsite.countryid, fips_code:unit.of.measure, potw.releases.underground.Iwells.offsite:energy)
#   ) %>%
#   data.frame()
#======================================================================================================================#
### Variable Creation
#======================================================================================================================#
triQ.on <- triQ.on %>%
  mutate(
	ind.output = vadd + prodh + matcost + energy,
	output.perworker = (ind.output / emp),
	output.perhr = (ind.output / prodh),
	wage.perhr = prodw / (prodh),
	energy.intensity = (energy / ind.output),
	total.air.emissions.onsite.intensity = (total.air.emissions.onsite / ind.output),
	total.fug.air.emissions.onsite.intensity = (total.fug.air.emissions.onsite / ind.output),
	total.point.air.emissions.onsite.intensity = (total.point.air.emissions.onsite / ind.output),
	total.landfills.onsite.intensity = (total.landfills.onsite / ind.output),
	total.land.releases.onsite.intensity = (total.land.releases.onsite / ind.output),
	total.surface.water.discharge.onsite.intensity = (total.surface.water.discharge.onsite / ind.output),
	total.releases.onsite.intensity = (total.releases.onsite / ind.output),
  )

sum_up(triQ.on,
	   c(total.fug.air.emissions.onsite.intensity, total.point.air.emissions.onsite.intensity,
		 total.air.emissions.onsite.intensity))

triQ.on %>%
  group_by(chemical.name) %>%
  summarise(air.emiss.intensity = sum(total.air.emissions.onsite.intensity, na.rm = TRUE)) %>%
  arrange(desc(air.emiss.intensity)) %>%
  print(., n = nrow(.))