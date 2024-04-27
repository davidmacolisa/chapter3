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
### Loading Data---Offsite
#======================================================================================================================#
start_time <- Sys.time()
triQ <- read_rds(file = "./Data_PhD/US/BLS/triQ.rds") %>%
  group_by(facility.state, year) %>%
  filter(offsite.zip.length == 5) %>%
  mutate(
    tot.ch.amt = ch.amt + sum2.sub.mw.ch,
    end.mw = start.mw + tot.ch.amt
  ) %>%
  select(c(
    year, facility.state, state, lat, long, offsite.id, offsite.facility.id, offsite.sequence.number, offsite.zipcode,
    offsite.city, offsite.county, offsite.state, offsite.zip.length, naics.code, industry.name, chemical.id,
    chemical.name, chemical.classification, unit.of.measure, contains(match = "offsite"), entire.facility,
    federal.facility, govt.owned.facility, comment.type, comment.type.description, comment.text, classification,
    clean.air.act.chems, carcinogenic.chems, metal.restrict.tri:pi.chem.facility,
    chemical.formulation.component:chemical.ancilliary.use, cpi:tfp5, population, treated:sum2.sub.mw.ch, tot.ch.amt,
    start.mw, end.mw, match.ch.amt, match.ch.year, dist.to.border
  )) %>%
  select(-contains(match = "potw")) %>%
  data.frame()
end_time <- Sys.time()
end_time - start_time
gc()

unique(triQ$chemical.ancilliary.use)
sum_up(triQ, total.releases.offsite)

triQc <- triQ %>%
  select(
    -c(comment.type, comment.type.description, comment.text, classification, production.or.activity)
  )

sum(is.na(triQc))
na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
triQc <- triQc[complete.cases(triQc),]
sort(unique(triQc$year))
sum(is.na(triQc))
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
### Keeping only common facility states across years---Panelize the facility.state
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "facility.state", year_var = "year")
check_ids(df = triQc, id_var = "facility.state", year_var = "year")
n_distinct(triQc$facility.state)
sort(unique(triQc$facility.state))
#======================================================================================================================#
### Keeping only common offsite ids and offsite facility.id across years---Panelize the offsite facility.ids
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "offsite.id", year_var = "year")
check_ids(df = triQc, id_var = "offsite.id", year_var = "year")
n_distinct(triQc$offsite.id)

triQc <- get_unique_ids(df = triQc, id_var = "offsite.facility.id", year_var = "year")
check_ids(df = triQc, id_var = "offsite.facility.id", year_var = "year")
n_distinct(triQc$offsite.facility.id)
#======================================================================================================================#
### Keeping only common offsite.zipcode across years---Panelize the offsite.zipcode
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "offsite.zipcode", year_var = "year")
check_ids(df = triQc, id_var = "offsite.zipcode", year_var = "year")
n_distinct(triQc$offsite.zipcode)
#======================================================================================================================#
### Keeping only common offsite.city across years---Panelize the offsite.city
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "offsite.city", year_var = "year")
check_ids(df = triQc, id_var = "offsite.city", year_var = "year")
n_distinct(triQc$offsite.city)
#======================================================================================================================#
### Keeping only common offsite.county across years---Panelize the offsite.county
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "offsite.county", year_var = "year")
check_ids(df = triQc, id_var = "offsite.county", year_var = "year")
n_distinct(triQc$offsite.county)
#======================================================================================================================#
### Keeping only common naics.code across years---Panelize the naics.code
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "naics.code", year_var = "year")
check_ids(df = triQc, id_var = "naics.code", year_var = "year")
n_distinct(triQc$naics.code)
#======================================================================================================================#
### Keeping only common chemical across years---Panelize the chemicals
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "chemical.id", year_var = "year")
check_ids(df = triQc, id_var = "chemical.id", year_var = "year")
n_distinct(triQc$chemical.id)
#======================================================================================================================#
check_ids(df = triQc, id_var = "offsite.id", year_var = "year")
check_ids(df = triQc, id_var = "offsite.facility.id", year_var = "year")
check_ids(df = triQc, id_var = "offsite.zipcode", year_var = "year")
check_ids(df = triQc, id_var = "offsite.city", year_var = "year")
check_ids(df = triQc, id_var = "offsite.county", year_var = "year")
check_ids(df = triQc, id_var = "naics.code", year_var = "year")
check_ids(df = triQc, id_var = "chemical.id", year_var = "year")
#======================================================================================================================#
sum_up(triQc, c(total.underground.injection.offsite, total.landfills.offsite, total.releases.toland.treatment.offsite,
                total.surface.impoundment.offsite, total.land.releases.other.offsite, total.land.releases.offsite,
                total.releases.storage.offsite, total.releases.metalsolidify.offsite, total.releases.other.mgt.offsite,
                total.releases.wastebroker.offsite, total.releases.unknown.offsite, total.releases.offsite,
                energy.recovery.offsite))

sum_up(triQc, c(total.underground.injection.offsite, total.landfills.offsite, total.releases.toland.treatment.offsite,
                total.surface.impoundment.offsite, total.land.releases.other.offsite, total.land.releases.offsite,
                total.releases.storage.offsite, total.releases.metalsolidify.offsite, total.releases.other.mgt.offsite,
                total.releases.wastebroker.offsite, total.releases.unknown.offsite, total.releases.offsite,
                energy.recovery.offsite))

sum_up(triQc, c(total.underground.injection.offsite, total.landfills.offsite, total.releases.toland.treatment.offsite,
                total.surface.impoundment.offsite, total.land.releases.other.offsite, total.land.releases.offsite,
                total.releases.storage.offsite, total.releases.metalsolidify.offsite, total.releases.other.mgt.offsite,
                total.releases.wastebroker.offsite, total.releases.unknown.offsite, total.releases.offsite,
                energy.recovery.offsite))

sort(unique(triQc$facility.state))
sort(unique(triQc[triQc$treated == 1,]$facility.state))
sort(unique(triQc[triQc$treated == 0,]$facility.state))
n_distinct(triQc$facility.state)
n_distinct(triQc$offsite.state)
n_distinct(triQc$treated.match)
n_distinct(triQc$control.match)
n_distinct(triQc$chemical.name)
sum_up(triQc %>% filter(treated == 0), c(ch.amt, sum2.sub.mw.ch, end.mw))

table(triQc$chemical.name)
#======================================================================================================================#
### For the state-level analysis---Onsite
### Collapse triQc to state level
#======================================================================================================================#
library(collapse)
triQs <- triQc %>%
  collap(
    X = .,
    by = ~
      offsite.sequence.number +
      offsite.facility.id +
        facility.state +
        offsite.state +
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
      entire.facility +
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
        own_code,
    na.rm = T,
    FUN = fsum,
    catFUN = fmode,
    keep.col.order = T,
    return = "long"
  ) %>%
  select(
    -c(Function, offsite.id, offsite.zipcode:offsite.county, offsite.zipcode, offsite.zip.length,
       treated.cluster.name:cbcp.id, treated.cluster.lat:control.cluster.long, dist.to.border, lat, long,
       federal.facility, govt.owned.facility)
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
table(triQs$chemical.ancilliary.use)

#======================================================================================================================#
### Converting variables in triQc to numeric
#======================================================================================================================#
triQc <- triQc %>%
  mutate(
    lat = as.numeric(lat),
    long = as.numeric(long),
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
table(triQs$chemical.ancilliary.use)
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
#======================================================================================================================#
triQc <- triQc %>%
  mutate(
    ind.output = vadd + prodh + matcost + energy,
    l.ind.output = log(ind.output),
    l.output.perworker = log((ind.output / emp) + 1),
    l.output.perhr = log((ind.output / prodh) + 1),
    l.wage.perhr = log((prodw / prodh) + 1),
    l.energy.intensity = log((energy / ind.output) + 1),
    l.total.underground.injection.offsite.intensity = log((total.underground.injection.offsite / ind.output) + 1),
    l.total.underground.injection.I.wells.offsite.intensity = log((total.underground.injection.I.wells.offsite / ind.output) + 1),
    l.total.underground.injection.I.IV.wells.offsite.intensity = log((total.underground.injection.I.IV.wells.offsite / ind.output) + 1),
    l.total.landfills.offsite.intensity = log((total.landfills.offsite / ind.output) + 1),
    l.total.releases.toland.treatment.offsite.intensity = log((total.releases.toland.treatment.offsite / ind.output) + 1),
    l.total.surface.impoundment.offsite.intensity = log((total.surface.impoundment.offsite / ind.output) + 1),
    l.total.surface.impoundment1.offsite.intensity = log((total.surface.impoundment1.offsite / ind.output) + 1),
    l.total.surface.impoundment2.offsite.intensity = log((total.surface.impoundment2.offsite / ind.output) + 1),
    l.total.land.releases.other.offsite.intensity = log((total.land.releases.other.offsite / ind.output) + 1),
    l.total.land.releases.offsite.intensity = log((total.land.releases.offsite / ind.output) + 1),
    l.total.releases.storage.offsite.intensity = log((total.releases.storage.offsite / ind.output) + 1),
    l.total.releases.metalsolidify.offsite.intensity = log((total.releases.metalsolidify.offsite / ind.output) + 1),
    l.total.releases.other.mgt.offsite.intensity = log((total.releases.other.mgt.offsite / ind.output) + 1),
    l.total.releases.wastebroker.offsite.intensity = log((total.releases.wastebroker.offsite / ind.output) + 1),
    l.total.releases.unknown.offsite.intensity = log((total.releases.unknown.offsite / ind.output) + 1),
    l.total.releases.offsite.intensity = log((total.releases.offsite / ind.output) + 1),
    l.energy.recovery.offsite = log((energy.recovery.offsite / ind.output) + 1),
    l.energy.recovery.wastebroker.offsite = log((energy.recovery.wastebroker.offsite / ind.output) + 1),
    l.total.energy.recovery.offsite = log((total.energy.recovery.offsite / ind.output) + 1),
    l.recycling.solventsorganic.offsite = log((recycling.solventsorganic.offsite / ind.output) + 1),
    l.recycling.metals.offsite = log((recycling.metals.offsite / ind.output) + 1),
    l.recycling.reuse.offsite = log((recycling.reuse.offsite / ind.output) + 1),
    l.recycling.acidgen.offsite = log((recycling.acidgen.offsite / ind.output) + 1),
    l.recycling.wastebroker.offsite = log((recycling.wastebroker.offsite / ind.output) + 1),
    l.total.recycling.offsite = log((total.recycling.offsite / ind.output) + 1),
    l.treatment.nonmetalsolidify.offsite = log((treatment.nonmetalsolidify.offsite / ind.output) + 1),
    l.incineration.thermal.treatment.offsite = log((incineration.thermal.treatment.offsite / ind.output) + 1),
    l.incineration.thermal.treatment.heatvalue.offsite = log((incineration.thermal.treatment.heatvalue.offsite / ind.output) + 1),
    l.wastewater.treatment.nonmetals.offsite = log((wastewater.treatment.nonmetals.offsite / ind.output) + 1),
    l.waste.treatment.other.offsite = log((waste.treatment.other.offsite / ind.output) + 1),
    l.waste.treatment.wastebroker.offsite = log((waste.treatment.wastebroker.offsite / ind.output) + 1),
    l.total.waste.treatment.offsite = log((total.waste.treatment.offsite / ind.output) + 1),
    l.total.waste.management.offsite = log((total.waste.management.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    # l.total_annual_wages = log(total_annual_wages + 1),
    # l.annual_avg_wkly_wage = log(annual_avg_wkly_wage + 1),
    # l.avg_annual_pay = log(avg_annual_pay + 1),
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

triQs <- triQs %>%
  mutate(
    ind.output = vadd + prodh + matcost + energy,
    l.ind.output = log(ind.output),
    l.output.perworker = log((ind.output / emp) + 1),
    l.output.perhr = log((ind.output / prodh) + 1),
    l.wage.perhr = log((prodw / prodh) + 1),
    l.energy.intensity = log((energy / ind.output) + 1),
    l.total.underground.injection.offsite.intensity = log((total.underground.injection.offsite / ind.output) + 1),
    l.total.underground.injection.I.wells.offsite.intensity = log((total.underground.injection.I.wells.offsite / ind.output) + 1),
    l.total.underground.injection.I.IV.wells.offsite.intensity = log((total.underground.injection.I.IV.wells.offsite / ind.output) + 1),
    l.total.landfills.offsite.intensity = log((total.landfills.offsite / ind.output) + 1),
    l.total.releases.toland.treatment.offsite.intensity = log((total.releases.toland.treatment.offsite / ind.output) + 1),
    l.total.surface.impoundment.offsite.intensity = log((total.surface.impoundment.offsite / ind.output) + 1),
    l.total.surface.impoundment1.offsite.intensity = log((total.surface.impoundment1.offsite / ind.output) + 1),
    l.total.surface.impoundment2.offsite.intensity = log((total.surface.impoundment2.offsite / ind.output) + 1),
    l.total.land.releases.other.offsite.intensity = log((total.land.releases.other.offsite / ind.output) + 1),
    l.total.land.releases.offsite.intensity = log((total.land.releases.offsite / ind.output) + 1),
    l.total.releases.storage.offsite.intensity = log((total.releases.storage.offsite / ind.output) + 1),
    l.total.releases.metalsolidify.offsite.intensity = log((total.releases.metalsolidify.offsite / ind.output) + 1),
    l.total.releases.other.mgt.offsite.intensity = log((total.releases.other.mgt.offsite / ind.output) + 1),
    l.total.releases.wastebroker.offsite.intensity = log((total.releases.wastebroker.offsite / ind.output) + 1),
    l.total.releases.unknown.offsite.intensity = log((total.releases.unknown.offsite / ind.output) + 1),
    l.total.releases.offsite.intensity = log((total.releases.offsite / ind.output) + 1),
    l.energy.recovery.offsite = log((energy.recovery.offsite / ind.output) + 1),
    l.energy.recovery.wastebroker.offsite = log((energy.recovery.wastebroker.offsite / ind.output) + 1),
    l.total.energy.recovery.offsite = log((total.energy.recovery.offsite / ind.output) + 1),
    l.recycling.solventsorganic.offsite = log((recycling.solventsorganic.offsite / ind.output) + 1),
    l.recycling.metals.offsite = log((recycling.metals.offsite / ind.output) + 1),
    l.recycling.reuse.offsite = log((recycling.reuse.offsite / ind.output) + 1),
    l.recycling.acidgen.offsite = log((recycling.acidgen.offsite / ind.output) + 1),
    l.recycling.wastebroker.offsite = log((recycling.wastebroker.offsite / ind.output) + 1),
    l.total.recycling.offsite = log((total.recycling.offsite / ind.output) + 1),
    l.treatment.nonmetalsolidify.offsite = log((treatment.nonmetalsolidify.offsite / ind.output) + 1),
    l.incineration.thermal.treatment.offsite = log((incineration.thermal.treatment.offsite / ind.output) + 1),
    l.incineration.thermal.treatment.heatvalue.offsite = log((incineration.thermal.treatment.heatvalue.offsite / ind.output) + 1),
    l.wastewater.treatment.nonmetals.offsite = log((wastewater.treatment.nonmetals.offsite / ind.output) + 1),
    l.waste.treatment.other.offsite = log((waste.treatment.other.offsite / ind.output) + 1),
    l.waste.treatment.wastebroker.offsite = log((waste.treatment.wastebroker.offsite / ind.output) + 1),
    l.total.waste.treatment.offsite = log((total.waste.treatment.offsite / ind.output) + 1),
    l.total.waste.management.offsite = log((total.waste.management.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
    l.total.landfills.offsite = log((total.landfills.offsite / ind.output) + 1),
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
#======================================================================================================================#
### Save data
#======================================================================================================================#
start_time <- Sys.time()
write_rds(triQc, file = "./Data_PhD/US/BLS/offsite/triQc_off.rds", compress = "xz")
write_rds(triQs, file = "./Data_PhD/US/BLS/offsite/triQs_off.rds", compress = "xz")
end_time <- Sys.time()
end_time - start_time
#======================================================================================================================#