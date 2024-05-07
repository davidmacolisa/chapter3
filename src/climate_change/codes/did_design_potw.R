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
  mutate(
    tot.ch.amt = ch.amt + sum2.sub.mw.ch,
    end.mw = start.mw + tot.ch.amt
  ) %>%
  select(c(
    year, facility.id, facility.zipcode, zip.length, facility.city, fips_code, facility.county, facility.state, state,
    lat, long, offsite.id, offsite.facility.id, offsite.sequence.number, offsite.zipcode, offsite.city, offsite.county,
    offsite.state, offsite.zip.length, potw.id, potw.zipcode, potw.zip.length, potw.city, potw.county, potw.state,
    naics.code:unit.of.measure, contains(match = "potw"), maxnum.chem.onsite, entire.facility, federal.facility,
    govt.owned.facility, comment.type, comment.type.description, comment.text, classification, clean.air.act.chems,
    carcinogenic.chems, metal.restrict.tri:pi.chem.facility, chemical.formulation.component:chemical.ancilliary.use,
    cpi, gdp, annual_avg_estabs, population, vadd, treated:sum2.sub.mw.ch, tot.ch.amt, start.mw, end.mw, match.ch.amt,
    match.ch.year, dist.to.border
  )) %>%
  select(-contains(match = "npotw")) %>%
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
triQc <- triQc[complete.cases(triQc$potw.zipcode),]
triQc <- triQc[complete.cases(triQc$facility.id),]
triQc <- triQc[complete.cases(triQc$offsite.id),]
triQc <- triQc[complete.cases(triQc$offsite.facility.id),]
sort(unique(triQc$year))
sum(is.na(triQc))
na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]

sum_up(triQc, potw.releases.underground.Iwells.offsite:total.potw.management.offsite)
#======================================================================================================================#
### Imputation with before and after ids
#======================================================================================================================#
sum(is.na(triQc$potw.id))
triQc_na <- triQc[triQc$potw.id == "NA",]
# triQc <- triQc[complete.cases(triQc$potw.zipcode),]
sort(unique(triQc$year))

# Install and load the zoo package if not already installed
if (!requireNamespace(package = "zoo", quietly = TRUE)) {
  install.packages(pkgs = "zoo")
}

library(zoo)
filled_data <- zoo::na.locf(triQc$potw.id, maxgap = max(triQc$potw.zipcode), na.rm = FALSE)
filled_data_after <- zoo::na.locf(triQc$potw.id, maxgap = max(triQc$potw.zipcode), fromLast = TRUE, na.rm = FALSE) # after

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

sum(is.na(triQc$maxnum.chem.onsite))
triQc_na <- triQc[triQc$maxnum.chem.onsite == "NA",]
triQc <- triQc[complete.cases(triQc$maxnum.chem.onsite),]
na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
sort(unique(triQc$year))

sum(is.na(triQc$gdp))
triQc_na <- triQc[triQc$gdp == "NA",]
triQc <- triQc[complete.cases(triQc$gdp),]
na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
sort(unique(triQc$year))

sum(is.na(triQc$annual_avg_estabs))
triQc_na <- triQc[triQc$annual_avg_estabs == "NA",]
triQc <- triQc[complete.cases(triQc$annual_avg_estabs),]
na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
sort(unique(triQc$year))

sum(is.na(triQc$vadd))
triQc_na <- triQc[triQc$vadd == "NA",]
triQc <- triQc[complete.cases(triQc$vadd),]
na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
sort(unique(triQc$year))
sum(is.na(triQc))

# sum(is.na(triQc$personal_income))
# triQc_na <- triQc[triQc$personal_income == "NA",]
# triQc <- triQc[complete.cases(triQc$personal_income),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))

# sum(is.na(triQc$own_code))
# triQc_na <- triQc[triQc$own_code == "NA",]
# triQc <- triQc[complete.cases(triQc$own_code),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))

# sum(is.na(triQc$emp))
# triQc_na <- triQc[triQc$emp == "NA",]
# triQc <- triQc[complete.cases(triQc$emp),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))
sum(is.na(triQc))

sort(unique(triQc$facility.state))
sort(unique(triQc$potw.state))
n_distinct(triQc$potw.id)
n_distinct(triQc$facility.state)
n_distinct(triQc$potw.state)
n_distinct(triQc$treated.match)
n_distinct(triQc$control.match)
n_distinct(triQc$chemical.id)
sum_up(triQc %>% filter(treated == 1), c(ch.amt, sum2.sub.mw.ch, end.mw))
sum_up(triQc %>% filter(treated == 0), c(ch.amt, sum2.sub.mw.ch, end.mw))

glimpse(triQc)
str(triQc)
#======================================================================================================================#
### check for zero columns
#======================================================================================================================#
zero_cols <- names(triQc)[colSums(triQc == 0) == nrow(triQc)]
triQc <- triQc[, !names(triQc) %in% zero_cols]
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
### Keeping only common facility.id across years---Panelize the facility.ids
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "facility.id", year_var = "year")
check_ids(df = triQc, id_var = "facility.id", year_var = "year")
n_distinct(triQc$facility.id)
#======================================================================================================================#
### Keeping only common chemical across years---Panelize the chemicals
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "chemical.id", year_var = "year")
check_ids(df = triQc, id_var = "chemical.id", year_var = "year")
n_distinct(triQc$chemical.id)
#======================================================================================================================#
check_ids(df = triQc, id_var = "facility.id", year_var = "year")
check_ids(df = triQc, id_var = "facility.zipcode", year_var = "year")
check_ids(df = triQc, id_var = "facility.city", year_var = "year")
check_ids(df = triQc, id_var = "fips_code", year_var = "year")
check_ids(df = triQc, id_var = "facility.county", year_var = "year")
check_ids(df = triQc, id_var = "chemical.id", year_var = "year")
check_ids(df = triQc, id_var = "facility.state", year_var = "year")
check_ids(df = triQc, id_var = "treated.match", year_var = "year")
check_ids(df = triQc, id_var = "treated.cluster.id", year_var = "year")
#======================================================================================================================#
check_ids(df = triQc, id_var = "potw.id", year_var = "year")
check_ids(df = triQc, id_var = "potw.zipcode", year_var = "year")
check_ids(df = triQc, id_var = "potw.city", year_var = "year")
check_ids(df = triQc, id_var = "potw.county", year_var = "year")
check_ids(df = triQc, id_var = "potw.state", year_var = "year")
#======================================================================================================================#
### For the state-level analysis---Onsite
### Collapse triQc to state level
#======================================================================================================================#
library(collapse)
triQs <- triQc %>%
  collap(
    X = .,
    by = ~
      year +
        facility.id +
        facility.zipcode +
        zip.length +
        facility.state +
        state +
        offsite.id +
        offsite.facility.id +
        offsite.zipcode +
        offsite.zip.length +
        offsite.sequence.number +
        offsite.state +
        potw.id +
        potw.zipcode +
        potw.zip.length +
        potw.state +
        naics.code +
        industry.name +
        chemical.id +
        chemical.name +
        chemical.classification +
        unit.of.measure +
        potw.releases.underground.Iwells.offsite +
        potw.releases.underground.other.offsite +
        total.potw.releases.offsite +
        potw.treatment.offsite +
        total.potw.management.offsite +
        maxnum.chem.onsite +
        entire.facility +
        federal.facility +
        clean.air.act.chems +
        carcinogenic.chems +
        metal.restrict.tri +
        production.ratio.activity.index +
        produced.chem.facility +
        imported.chem.facility +
        pi.chem.facility +
        chemical.formulation.component +
        chemical.article.component +
        chemical.manufacturing.aid +
        chemical.ancilliary.use +
        cpi +
        gdp +
        annual_avg_estabs +
        population +
        vadd +
        treated +
        treated.match +
        control.match +
        overlap +
        state.border.id +
        ch.year +
        ch.amt +
        sum2.sub.mw.ch +
        tot.ch.amt +
        start.mw +
        end.mw +
        match.ch.amt +
        match.ch.year +
        dist.to.border,
    na.rm = T,
    FUN = fsum,
    catFUN = fmode,
    keep.col.order = T,
    return = "long"
  ) %>%
  select(
    -c(Function, facility.city, fips_code, facility.county, offsite.city, offsite.county, potw.city, potw.county,
       industry.category, lat, long, treated.cluster.name:control.cluster.long)
  ) %>%
  mutate(
    entire.facility = as.numeric(entire.facility),
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
sum(is.na(triQs))
na_columns <- colnames(triQs)[colSums(is.na(triQs)) > 0]
#======================================================================================================================#
### Converting variables in triQc to numeric
#======================================================================================================================#
triQc <- triQc %>%
  mutate(
    entire.facility = as.numeric(entire.facility),
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
    ind.output.lb = vadd / 454,
    l.ind.output.lb = log(ind.output.lb),
    potw.releases.underground.Iwells.offsite.intensity = potw.releases.underground.Iwells.offsite / ind.output.lb,
    l.potw.releases.underground.Iwells.offsite.intensity = log((potw.releases.underground.Iwells.offsite / ind.output.lb) + 1),
    potw.releases.underground.other.offsite.intensity = potw.releases.underground.other.offsite / ind.output.lb,
    l.potw.releases.underground.other.offsite.intensity = log((potw.releases.underground.other.offsite / ind.output.lb) + 1),
    total.potw.releases.offsite.intensity = total.potw.releases.offsite / ind.output.lb,
    l.total.potw.releases.offsite.intensity = log((total.potw.releases.offsite / ind.output.lb) + 1),
    l.potw.treatment.offsite = log(potw.treatment.offsite + 1),
    l.total.potw.management.offsite = log(total.potw.management.offsite + 1),
    gdp.pc = gdp / population,
    gdppc.1 = stats::lag(gdp.pc, k = 1),
    cpi.1 = stats::lag(cpi, k = 1),
    annual.avg.estabs.1 = stats::lag(annual_avg_estabs, k = 1)
  )

triQs <- triQs %>%
  mutate(
    ind.output.lb = vadd / 454,
    l.ind.output.lb = log(ind.output.lb),
    potw.releases.underground.Iwells.offsite.intensity = potw.releases.underground.Iwells.offsite / ind.output.lb,
    l.potw.releases.underground.Iwells.offsite.intensity = log((potw.releases.underground.Iwells.offsite / ind.output.lb) + 1),
    potw.releases.underground.other.offsite.intensity = potw.releases.underground.other.offsite / ind.output.lb,
    l.potw.releases.underground.other.offsite.intensity = log((potw.releases.underground.other.offsite / ind.output.lb) + 1),
    total.potw.releases.offsite.intensity = total.potw.releases.offsite / ind.output.lb,
    l.total.potw.releases.offsite.intensity = log((total.potw.releases.offsite / ind.output.lb) + 1),
    l.potw.treatment.offsite = log(potw.treatment.offsite + 1),
    l.total.potw.management.offsite = log(total.potw.management.offsite + 1),
    gdp.pc = gdp / population,
    gdppc.1 = stats::lag(gdp.pc, k = 1),
    cpi.1 = stats::lag(cpi, k = 1),
    annual.avg.estabs.1 = stats::lag(annual_avg_estabs, k = 1)
  )
sort(unique(triQs$year))
#======================================================================================================================#
### Experimental design
#======================================================================================================================#
triQc <- triQc %>%
  rename(fips.code = fips_code) %>%
  mutate(
    hap.chems = case_when(chemical.classification == "TRI" ~ 1, T ~ 0),
    # dioxin.chems = case_when(chemical.classification == "Dioxin" ~ 1, T ~ 0),
    pbt.chems = case_when(chemical.classification == "PBT" ~ 1, T ~ 0),
  ) %>%
  mutate(
    e.treated = case_when(year >= ch.year ~ 1, T ~ 0), #states e-years away from the initial treatment year
    post = case_when(year == 2014 | year == 2015 | year == 2017 ~ 1, T ~ 0),
    rel.year = year - ch.year + 2014,
    facility.id = as.numeric(facility.id),
    naics.code = as.numeric(naics.code),
    fips.code = as.numeric(fips.code),
    # offsite.state.id = as.numeric(as.factor(offsite.state)),
    potw.id = as.numeric(potw.id),
    potw.state.id = as.numeric(as.factor(potw.state)),
    facility.state.id = as.numeric(as.factor(facility.state)),
    treated.cluster.id = as.numeric(treated.cluster.id),
    control.cluster.id = as.numeric(control.cluster.id),
    fips.state.fe = as.numeric(fips.code) * as.numeric(facility.state.id),
    fac.chem.fe = as.numeric(facility.id) * as.numeric(as.factor(chemical.id)),
    facility.year.fe = as.numeric(facility.id) * year,
    chemical.year.fe = as.numeric(as.factor(chemical.id)) * year,
    fips.year.fe = as.numeric(fips.code) * year,
    state.year.fe = as.numeric(facility.state.id) * year,
    treated.cluster.year.fe = as.numeric(treated.cluster.id) * year,
    control.cluster.year.fe = as.numeric(control.cluster.id) * year
  )

triQs <- triQs %>%
  mutate(
    hap.chems = case_when(chemical.classification == "TRI" ~ 1, T ~ 0),
    # dioxin.chems = case_when(chemical.classification == "Dioxin" ~ 1, T ~ 0),
    pbt.chems = case_when(chemical.classification == "PBT" ~ 1, T ~ 0),
  ) %>%
  mutate(
    e.treated = case_when(year >= ch.year ~ 1, T ~ 0), #states e-years away from the initial treatment year
    post = case_when(year == 2014 | year == 2015 | year == 2017 ~ 1, T ~ 0),
    rel.year = year - ch.year + 2014,
    facility.id = as.numeric(facility.id),
    naics.code = as.numeric(naics.code),
    # offsite.state.id = as.numeric(as.factor(offsite.state)),
    potw.id = as.numeric(potw.id),
    potw.state.id = as.numeric(as.factor(offsite.state)),
    facility.state.id = as.numeric(as.factor(facility.state)),
    treated.match.fe = as.numeric(as.factor(treated.match)),
    control.match.fe = as.numeric(as.factor(control.match)),
    fac.chem.fe = as.numeric(facility.id) * as.numeric(as.factor(chemical.id)),
    facility.year.fe = as.numeric(facility.id) * year,
    chemical.year.fe = as.numeric(as.factor(chemical.id)) * year,
    state.year.fe = as.numeric(facility.state.id) * year,
    treated.match.year.fe = as.numeric(as.factor(treated.match)) * year,
    control.match.year.fe = as.numeric(as.factor(control.match)) * year
  )
#======================================================================================================================#
### check for zero columns
#======================================================================================================================#
zero_cols <- names(triQc)[colSums(triQc == 0) == nrow(triQc)]
triQc <- triQc[, !names(triQc) %in% zero_cols]

zero_cols <- names(triQs)[colSums(triQs == 0) == nrow(triQs)]
triQs <- triQs[, !names(triQs) %in% zero_cols]
#======================================================================================================================#
### Save data
#======================================================================================================================#
start_time <- Sys.time()
write_rds(triQc, file = "./Data_PhD/US/BLS/offsite/triQc_potw.rds", compress = "xz")
write_rds(triQs, file = "./Data_PhD/US/BLS/offsite/triQs_potw.rds", compress = "xz")
end_time <- Sys.time()
end_time - start_time
#======================================================================================================================#