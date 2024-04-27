#======================================================================================================================#
### Packages
#======================================================================================================================#
library(tidyverse)
library(statar)
library(usgeogr)
# install.packages("remotes")
# remotes::install_github("davidsovich/usgeogr")
#======================================================================================================================#
### Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading Data---Offsite
#======================================================================================================================#
start_time <- Sys.time()
triQ <- read_rds(file = "./Data_PhD/US/BLS/triQ.rds") %>%
  group_by(facility.county) %>%
  filter(potw.zip.length == 5) %>%
  mutate(
    tot.ch.amt = ch.amt + sum2.sub.mw.ch,
    end.mw = start.mw + tot.ch.amt
  ) %>%
  select(c(
    year, offsite.state, facility.state, facility.county, state, lat, long, potw.id, potw.zipcode, potw.city,
    potw.county, naics.code:unit.of.measure, potw.state, potw.zip.length, contains(match = "potw"), entire.facility,
    govt.owned.facility, clean.air.act.chems, carcinogenic.chems, metal.restrict.tri, produced.chem.facility,
    production.ratio.activity.index, production.or.activity, imported.chem.facility, pi.chem.facility,
    chemical.formulation.component, chemical.article.component, chemical.manufacturing.aid, chemical.ancilliary.use,
    comment.type, comment.type.description, comment.text, classification, cpi:tfp5, population, treated:sum2.sub.mw.ch,
    tot.ch.amt, start.mw, end.mw, match.ch.amt, match.ch.year, dist.to.border
  )) %>%
  data.frame()
end_time <- Sys.time()
end_time - start_time
gc()

triQc <- triQ %>%
  select(
    -c(comment.type, comment.type.description, comment.text, classification, production.or.activity)
  )

sum(is.na(triQc))
na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
sort(unique(triQc$year))
sum(is.na(triQc))

sum_up(triQc, potw.releases.underground.Iwells.offsite:total.potw.management.offsite)
#======================================================================================================================#
### Imputation with before and after ids
#======================================================================================================================#
sum(is.na(triQc$potw.id))
triQc_na <- triQc[triQc$potw.id == "NA",]
# triQc <- triQc[complete.cases(triQc$potw.id),]

# Install and load the zoo package if not already installed
if (!requireNamespace(package = "zoo", quietly = TRUE)) {
  install.packages(pkgs = "zoo")
}

library(zoo)
# Fill missing values with the last observed value (before) and next observed value (after)
filled_data <- zoo::na.locf(triQc$potw.id, na.rm = FALSE) # before
filled_data_after <- zoo::na.locf(triQc$potw.id, fromLast = TRUE, na.rm = FALSE) # after

# Combine before and after to get filled values
filled_data <- ifelse(test = is.na(filled_data), yes = filled_data_after, no = filled_data)

# Replace the original column with filled values
triQc$potw.id <- filled_data

na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
sort(unique(triQc$year))
#======================================================================================================================#
sum(is.na(triQc$production.ratio.activity.index))
triQc_na <- triQc[triQc$production.ratio.activity.index == "NA",]
triQc <- triQc[complete.cases(triQc$production.ratio.activity.index),]
na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
sort(unique(triQc$year))

sum(is.na(triQc$personal_income))
triQc_na <- triQc[triQc$personal_income == "NA",]
triQc <- triQc[complete.cases(triQc$personal_income),]
na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
sort(unique(triQc$year))

sum(is.na(triQc$own_code))
triQc_na <- triQc[triQc$own_code == "NA",]
triQc <- triQc[complete.cases(triQc$own_code),]
na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
sort(unique(triQc$year))

sum(is.na(triQc$emp))
triQc_na <- triQc[triQc$emp == "NA",]
triQc <- triQc[complete.cases(triQc$emp),]
na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
sort(unique(triQc$year))

sort(unique(triQc$year))
sort(unique(triQc$facility.state))
sort(unique(triQc$potw.state))
n_distinct(triQc$potw.id)
n_distinct(triQc$facility.state)
n_distinct(triQc$potw.state)
n_distinct(triQc$treated.match)
n_distinct(triQc$control.match)
n_distinct(triQc$chemical.id)
sum_up(triQc %>% filter(treated == 0), c(ch.amt, sum2.sub.mw.ch, end.mw))

glimpse(triQc)
str(triQc)
#======================================================================================================================#
### Functions to panelize the IDs
#======================================================================================================================#
# Function to get unique IDs across years
get_unique_ids <- function(df, id_var, year_var) {
  # order the df by year
  df <- df[order(df[[year_var]]),]

  # Get unique IDs for each year
  unique_ids_by_year <- split(df[[id_var]], df[[year_var]])
  unique_ids <- lapply(unique_ids_by_year, unique)

  # Get the IDs from the first year
  ids_year1 <- unique_ids[[1]]

  # Check if these IDs are in all other years
  ids_in_all_years <- Reduce(f = intersect, x = unique_ids)

  # Subset the data frame to keep only these IDs
  df_subset <- df[df[[id_var]] %in% ids_in_all_years,]

  return(df_subset)
}

# Function to check if IDs in the first year are the same across all years
check_ids <- function(df, id_var, year_var) {
  # order the df by year
  df <- df[order(df[[year_var]]),]

  # Get unique IDs for each year
  unique_ids <- split(df[[id_var]], df[[year_var]])
  unique_ids <- lapply(unique_ids, unique)

  # Get the IDs from the first year
  ids_year1 <- unique_ids[[1]]

  # Check if these IDs are in all other years
  ids_in_all_years <- Reduce(intersect, unique_ids)

  # Check if the IDs from the first year are the same as the IDs in all years
  same_ids <- all(ids_year1 %in% ids_in_all_years)

  # Print a statement saying whether or not the IDs are the same across years
  if (same_ids) {
    print("The IDs in the first year are the same across all other years.")
  } else {
    print("The IDs in the first year are not the same across all other years.")
  }
}

#======================================================================================================================#
### Keeping only common offsite potw.ids across years---Panelize the potw.ids
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "potw.id", year_var = "year")
check_ids(df = triQc, id_var = "potw.id", year_var = "year")
n_distinct(triQc$potw.id)
#======================================================================================================================#
### Keeping only common facility states across years---Panelize the facility.state
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "facility.state", year_var = "year")
check_ids(df = triQc, id_var = "facility.state", year_var = "year")
n_distinct(triQc$facility.state)
sort(unique(triQc$facility.state))
#======================================================================================================================#
### Keeping only common chemical across years---Panelize the chemicals
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "chemical.id", year_var = "year")
check_ids(df = triQc, id_var = "chemical.id", year_var = "year")
n_distinct(triQc$chemical.id)
#======================================================================================================================#
check_ids(df = triQc, id_var = "potw.id", year_var = "year")
check_ids(df = triQc, id_var = "facility.state", year_var = "year")
check_ids(df = triQc, id_var = "chemical.id", year_var = "year")
#======================================================================================================================#
### For the state-level analysis---Onsite
### Collapse triQc to state level
#======================================================================================================================#
library(collapse)
triQs <- triQc %>%
  collap(
    X = .,
    by = ~potw.id +
      facility.state +
      potw.state +
      state +
      year +
      treated +
      treated.match +
      control.match +
      overlap +
      state.border.id +
      ch.year +
      ch.amt +
      sum2.sub.mw.ch +
      start.mw +
      end.mw +
      match.ch.amt +
      match.ch.year +
      naics.code +
      industry.name +
      chemical.id +
      chemical.name +
      chemical.classification +
      unit.of.measure +
      potw.zip.length +
      entire.facility +
      govt.owned.facility +
      clean.air.act.chems +
      carcinogenic.chems +
      metal.restrict.tri +
      produced.chem.facility +
      imported.chem.facility +
      pi.chem.facility +
      chemical.formulation.component +
      chemical.article.component +
      chemical.manufacturing.aid +
      chemical.ancilliary.use +
      bea_unit +
      bea_rpp_unit +
      own_code +
      dist.to.border,
    na.rm = T,
    FUN = fsum,
    catFUN = fmode,
    keep.col.order = T,
    return = "long"
  ) %>%
  select(
    -c(Function, lat:long, potw.zipcode:potw.county, industry.category,
       treated.cluster.name:cbcp.id, treated.cluster.lat:control.cluster.long)
  ) %>%
  mutate(
    entire.facility = as.numeric(entire.facility),
    govt.owned.facility = as.numeric(govt.owned.facility),
    clean.air.act.chems = as.numeric(clean.air.act.chems),
    carcinogenic.chems = as.numeric(carcinogenic.chems),
    metal.restrict.tri = as.numeric(metal.restrict.tri),
    produced.chem.facility = as.numeric(produced.chem.facility),
    imported.chem.facility = as.numeric(imported.chem.facility),
    pi.chem.facility = as.numeric(pi.chem.facility),
    chemical.formulation.component = as.numeric(chemical.formulation.component),
    chemical.article.component = as.numeric(chemical.article.component),
    chemical.manufacturing.aid = as.numeric(chemical.manufacturing.aid),
    chemical.ancilliary.use = as.numeric(chemical.ancilliary.use)
  )
#======================================================================================================================#
### Converting variables in triQc to numeric
#======================================================================================================================#
triQc <- triQc %>%
  mutate(
    entire.facility = as.numeric(entire.facility),
    govt.owned.facility = as.numeric(govt.owned.facility),
    clean.air.act.chems = as.numeric(clean.air.act.chems),
    carcinogenic.chems = as.numeric(carcinogenic.chems),
    metal.restrict.tri = as.numeric(metal.restrict.tri),
    produced.chem.facility = as.numeric(produced.chem.facility),
    imported.chem.facility = as.numeric(imported.chem.facility),
    pi.chem.facility = as.numeric(pi.chem.facility),
    chemical.formulation.component = as.numeric(chemical.formulation.component),
    chemical.article.component = as.numeric(chemical.article.component),
    chemical.manufacturing.aid = as.numeric(chemical.manufacturing.aid),
    chemical.ancilliary.use = as.numeric(chemical.ancilliary.use)
  )
glimpse(triQc)
glimpse(triQs)
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
### Variables Creation
### Log Transformations with x + 1 to correct for 0
#======================================================================================================================#
triQc <- triQc %>%
  mutate(
    ind.output = vadd + prodh + matcost + energy,
    l.ind.output = log(ind.output),
    l.output.perworker = log((ind.output / emp) + 1),
    l.output.perhr = log((ind.output / prodh) + 1),
    l.wage.perhr = log((prodw / prodh) + 1),
    l.energy.intensity = log((energy / ind.output) + 1),
    l.potw.releases.underground.Iwells.offsite.intensity = log((potw.releases.underground.Iwells.offsite / ind.output) + 1),
    l.potw.releases.underground.other.offsite.intensity = log((potw.releases.underground.other.offsite / ind.output) + 1),
    l.total.potw.releases.offsite.intensity = log((total.potw.releases.offsite / ind.output) + 1),
    l.total.wastewater.releases.npotw.offsite.intensity = log((total.wastewater.releases.npotw.offsite / ind.output) + 1),
    l.potw.treatment.offsite.intensity = log((potw.treatment.offsite / ind.output) + 1),
    l.total.potw.management.offsite.intensity = log((total.potw.management.offsite / ind.output) + 1),
    l.total_annual_wages = log(total_annual_wages + 1),
    l.annual_avg_wkly_wage = log(annual_avg_wkly_wage + 1),
    l.avg_annual_pay = log(avg_annual_pay + 1),
    l.annual_avg_emplvl = log(annual_avg_emplvl + 1),
    l.emp = log(emp),
    l.pay = log(pay),
    l.prode = log(prode),
    l.prodh = log(prodh),
    l.prodw = log(prodw),
    l.revenue = log(vship),
    l.matcost = log(matcost),
    l.invest = log(invest),
    l.invent = log(invent),
    l.revenue = log(vship),
    l.cap = log(cap),
    l.equip = log(equip),
    l.plant = log(plant),
    l.tfp4 = log(tfp4),
    l.tfp5 = log(tfp5),
  )
sum_up(triQc, ind.output:l.plant)

triQs <- triQs %>%
  mutate(
    ind.output = vadd + prodh + matcost + energy,
    l.ind.output = log(ind.output),
    l.output.perworker = log((ind.output / emp) + 1),
    l.output.perhr = log((ind.output / prodh) + 1),
    l.wage.perhr = log((prodw / prodh) + 1),
    l.energy.intensity = log((energy / ind.output) + 1),
    l.potw.releases.underground.Iwells.offsite.intensity = log((potw.releases.underground.Iwells.offsite / ind.output) + 1),
    l.potw.releases.underground.other.offsite.intensity = log((potw.releases.underground.other.offsite / ind.output) + 1),
    l.total.potw.releases.offsite.intensity = log((total.potw.releases.offsite / ind.output) + 1),
    l.total.wastewater.releases.npotw.offsite = log((total.wastewater.releases.npotw.offsite / ind.output) + 1),
    l.potw.treatment.offsite = log((potw.treatment.offsite / ind.output) + 1),
    l.total.potw.management.offsite = log((total.potw.management.offsite / ind.output) + 1),
    l.total_annual_wages = log(total_annual_wages + 1),
    l.annual_avg_wkly_wage = log(annual_avg_wkly_wage + 1),
    l.avg_annual_pay = log(avg_annual_pay + 1),
    l.annual_avg_emplvl = log(annual_avg_emplvl + 1),
    l.emp = log(emp),
    l.pay = log(pay),
    l.prode = log(prode),
    l.prodh = log(prodh),
    l.prodw = log(prodw),
    l.revenue = log(vship),
    l.matcost = log(matcost),
    l.invest = log(invest),
    l.invent = log(invent),
    l.revenue = log(vship),
    l.cap = log(cap),
    l.equip = log(equip),
    l.plant = log(plant),
    l.tfp4 = log(tfp4),
    l.tfp5 = log(tfp5),
  )
sort(unique(triQs$year))
sum_up(triQc, emp)
sum_up(triQs, ind.output:l.plant)
#======================================================================================================================#
### Experimental design
#======================================================================================================================#
### For the county level
#======================================================================================================================#
table(triQc$facility.state, triQc$ch.year)
triQc <- triQc %>%
  mutate(
    e.treated = ifelse(test = year >= ch.year, yes = 1, no = 0), #states e-years away from the initial treatment year
    post = ifelse(test = year == 2014 | year == 2015 | year == 2017, yes = 1, no = 0),
    rel.year = year - ch.year + 2014
  )
sort(unique(triQc$year))
sort(unique(triQc$rel.year))
#======================================================================================================================#
### Save data
#======================================================================================================================#
start_time <- Sys.time()
write_rds(triQc, file = "./Data_PhD/US/BLS/offsite/triQc_potw.rds", compress = "xz")
write_rds(triQs, file = "./Data_PhD/US/BLS/offsite/triQs_potw.rds", compress = "xz")
end_time <- Sys.time()
end_time - start_time
#======================================================================================================================#