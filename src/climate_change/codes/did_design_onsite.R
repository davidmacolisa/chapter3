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
### Loading Data---Onsite
#======================================================================================================================#
start_time <- Sys.time()
triQ <- read_rds(file = "./Data_PhD/US/BLS/triQ.rds") %>%
  group_by(fips_code, year) %>%
  mutate(
    tot.ch.amt = ch.amt + sum2.sub.mw.ch,
    end.mw = start.mw + tot.ch.amt,
    lat = as.character(lat),
    long = as.character(long)
  ) %>%
  select(
    c(
      year, facility.id, facility.zipcode, facility.city, facility.county, fips_code, facility.state, state,
      lat, long, zip.length, naics.code, industry.name, chemical.id, chemical.name, chemical.classification,
      unit.of.measure,
      # air pollution emissions
      total.fug.air.emissions.onsite, total.point.air.emissions.onsite, total.air.emissions.onsite,
      # water pollution
      total.num.receiving.streams.onsite, total.surface.water.discharge.onsite,
      # land pollution
      total.underground.injection.I.wells.onsite, total.underground.injection.I.IV.wells.onsite,
      total.underground.injection.onsite, total.landfills.onsite, total.releases.toland.treatment.onsite,
      total.surface.impoundment.onsite, total.land.releases.other.onsite, total.land.releases.onsite,
      # all releases onsite
      total.releases.onsite,

      # onsite waste management
      energy.recovery.onsite, industrial.kiln.onsite, industrial.furnace.onsite, industrial.boiler.onsite,
      # recycling
      recycling.onsite, metal.recovery.onsite, solvent.recovery.onsite, reuse.onsite,
      # reuse
      reuse.onsite, biological.treatment.onsite, chemical.treatment.onsite,
      incineration.thermal.treatment.onsite, physical.treatment.onsite, material.subandmod,
      # onsite chemical production
      produced.chem.facility, imported.chem.facility, pi.chem.facility,
      # treatment onsite
      treatment.onsite, air.emissions.treatment.onsite, biological.treatment.onsite, chemical.treatment.onsite,
      incineration.thermal.treatment.onsite, physical.treatment.onsite,
      # overall onsite treatment: sum of energy reovery, recycling and treatment.onsite
      total.waste.management.onsite,
      # Other onsite mechanisms - onsite Source or Pollution Reduction Activities
      material.subandmod:intro.inline.productquality.process.analysis.opt,
      # placebo---when included, keep only complete cases in the dataset. This would reduce sample size
      total.release.onsite.catastrophicevents,
      # control variables
      maxnum.chem.onsite:chemical.ancilliary.use,
      # macro controls (including state-level regional price parity and population), county level wages and contributions,
      # establishments, employment, and industry-level variables (total employment, pay, production workers,
      # production worker hours, production workers wages, value of shipments, material cost, value added,
      # investments, inventories, energy cost, capital, equipment, plant, and total factor productivity growth rate)
      cpi:tfp5, population, treated:sum2.sub.mw.ch, tot.ch.amt, start.mw, end.mw, match.ch.amt,
      match.ch.year, dist.to.border
    )) %>%
  data.frame()
end_time <- Sys.time()
end_time - start_time
gc()

triQc <- triQ %>%
  select(
    -c(comment.type, comment.type.description, comment.text, classification, total.release.onsite.catastrophicevents,
       production.or.activity)
  )

sum(is.na(triQc))
na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
triQc <- triQc[complete.cases(triQc),]
sort(unique(triQc$year))
sum(is.na(triQc))

# sum(is.na(triQc$facility.id))
# triQc_na <- triQc[triQc$facility.id == "NA",]
# triQc <- triQc[complete.cases(triQc$facility.id),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))
#
# sum(is.na(triQc$production.ratio.activity.index))
# triQc_na <- triQc[triQc$production.ratio.activity.index == "NA",]
# triQc <- triQc[complete.cases(triQc$production.ratio.activity.index),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))
#
# sum(is.na(triQc$maxnum.chem.onsite))
# triQc_na <- triQc[triQc$maxnum.chem.onsite == "NA",]
# triQc <- triQc[complete.cases(triQc$maxnum.chem.onsite),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))
#
# sum(is.na(triQc$personal_income))
# triQc_na <- triQc[triQc$personal_income == "NA",]
# triQc <- triQc[complete.cases(triQc$personal_income),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))
#
# sum(is.na(triQc$own_code))
# triQc_na <- triQc[triQc$own_code == "NA",]
# triQc <- triQc[complete.cases(triQc$own_code),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))
#
# sum(is.na(triQc$emp))
# triQc_na <- triQc[triQc$emp == "NA",]
# triQc <- triQc[complete.cases(triQc$emp),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))
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
### Keeping only common facility.city across years---Panelize the facility.city
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "facility.city", year_var = "year")
check_ids(df = triQc, id_var = "facility.city", year_var = "year")
n_distinct(triQc$facility.city)
#======================================================================================================================#
### Keeping only common naics.code across years---Panelize the naics.code
#======================================================================================================================#
# triQc <- get_unique_ids(df = triQc, id_var = "naics.code", year_var = "year")
# check_ids(df = triQc, id_var = "naics.code", year_var = "year")
# n_distinct(triQc$naics.code)
#======================================================================================================================#
### Keeping only common chemical across years---Panelize the chemicals
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "chemical.id", year_var = "year")
check_ids(df = triQc, id_var = "chemical.id", year_var = "year")
n_distinct(triQc$chemical.id)
#======================================================================================================================#
### Keeping only common facility states across years---Panelize the facility.state
#======================================================================================================================#
#triQc <- get_unique_ids(df = triQc, id_var = "facility.state", year_var = "year")
check_ids(df = triQc, id_var = "facility.state", year_var = "year")
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
sum_up(triQc %>% filter(treated == 1),
       c(total.fug.air.emissions.onsite, total.point.air.emissions.onsite, total.air.emissions.onsite,
         total.num.receiving.streams.onsite, total.surface.water.discharge.onsite,
         total.underground.injection.I.wells.onsite, total.underground.injection.I.IV.wells.onsite,
         total.underground.injection.onsite, total.landfills.onsite, total.releases.toland.treatment.onsite,
         total.surface.impoundment.onsite, total.land.releases.other.onsite, total.land.releases.onsite))

sum_up(triQc %>% filter(treated == 0),
       c(total.fug.air.emissions.onsite, total.point.air.emissions.onsite, total.air.emissions.onsite,
         total.num.receiving.streams.onsite, total.surface.water.discharge.onsite,
         total.underground.injection.I.wells.onsite, total.underground.injection.I.IV.wells.onsite,
         total.underground.injection.onsite, total.landfills.onsite, total.releases.toland.treatment.onsite,
         total.surface.impoundment.onsite, total.land.releases.other.onsite, total.land.releases.onsite))

sort(unique(triQc$year))
sort(unique(triQc$state))
sort(unique(triQc$chemical.name))
n_distinct(triQc$state)
n_distinct(triQc$facility.id)
n_distinct(triQc$facility.zipcode)
n_distinct(triQc$facility.city)
n_distinct(triQc$fips_code)
n_distinct(triQc$facility.county)
n_distinct(triQc$chemical.name)
n_distinct(triQc$naics.code)
n_distinct(triQc$industry.name)
#======================================================================================================================#
### Converting variables in triQc to numeric
#======================================================================================================================#
triQc <- triQc %>%
  mutate(
    lat = as.numeric(lat),
    long = as.numeric(long),
    industrial.kiln.onsite = as.numeric(industrial.kiln.onsite),
    industrial.furnace.onsite = as.numeric(industrial.furnace.onsite),
    industrial.boiler.onsite = as.numeric(industrial.boiler.onsite),
    metal.recovery.onsite = as.numeric(metal.recovery.onsite),
    solvent.recovery.onsite = as.numeric(solvent.recovery.onsite),
    reuse.onsite = as.numeric(reuse.onsite),
    biological.treatment.onsite = as.numeric(biological.treatment.onsite),
    chemical.treatment.onsite = as.numeric(chemical.treatment.onsite),
    incineration.thermal.treatment.onsite = as.numeric(incineration.thermal.treatment.onsite),
    physical.treatment.onsite = as.numeric(physical.treatment.onsite),
    material.subandmod = as.numeric(material.subandmod),
    sub.fuel.matsubmod = as.numeric(sub.fuel.matsubmod),
    sub.organic.solvent.matsubmod = as.numeric(sub.organic.solvent.matsubmod),
    sub.rawm.feedstock.reactchem.matsubmod = as.numeric(sub.rawm.feedstock.reactchem.matsubmod),
    sub.manu.proccess.ancilliary.chems.matsubmod = as.numeric(sub.manu.proccess.ancilliary.chems.matsubmod),
    mod.content.grade.purity.chems.matsubmod = as.numeric(mod.content.grade.purity.chems.matsubmod),
    other.matmods.matsubmod = as.numeric(other.matmods.matsubmod),
    product.modification = as.numeric(product.modification),
    devd.newproductline.pmod = as.numeric(devd.newproductline.pmod),
    alt.dim.comp.design.pmod = as.numeric(alt.dim.comp.design.pmod),
    mod.packaging.pmod = as.numeric(mod.packaging.pmod),
    other.pmods.pmod = as.numeric(other.pmods.pmod),
    process.equip.modification = as.numeric(process.equip.modification),
    optimised.process.efficiency.pequipmod = as.numeric(optimised.process.efficiency.pequipmod),
    recirculationinprocess.pequipmod = as.numeric(recirculationinprocess.pequipmod),
    newtech.technique.process.pequipmod = as.numeric(newtech.technique.process.pequipmod),
    equipment.upgrade.update.pequipmod = as.numeric(equipment.upgrade.update.pequipmod),
    other.pequipmods.pequipmod = as.numeric(other.pequipmods.pequipmod),
    inventory.material.mgt = as.numeric(inventory.material.mgt),
    better.labelling.testing.immgt = as.numeric(better.labelling.testing.immgt),
    containers.sizechange.immgt = as.numeric(containers.sizechange.immgt),
    improved.materialhandling.operations.immgt = as.numeric(improved.materialhandling.operations.immgt),
    improved.monitoring.immgt = as.numeric(improved.monitoring.immgt),
    other.immgts.immgt = as.numeric(other.immgts.immgt),
    operating.practices.training = as.numeric(operating.practices.training),
    improved.schdule.operation.procedures.opt = as.numeric(improved.schdule.operation.procedures.opt),
    changed.production.schedule.opt = as.numeric(changed.production.schedule.opt),
    intro.inline.productquality.process.analysis.opt = as.numeric(intro.inline.productquality.process.analysis.opt),
    trade.secret = as.numeric(trade.secret),
    sanitised = as.numeric(sanitised),
    entire.facility = as.numeric(entire.facility),
    federal.facility = as.numeric(federal.facility),
    govt.owned.facility = as.numeric(govt.owned.facility),
    elemental.metal.included = as.numeric(elemental.metal.included),
    clean.air.act.chems = as.numeric(clean.air.act.chems),
    carcinogenic.chems = as.numeric(carcinogenic.chems),
    metal.restrict.tri = as.numeric(metal.restrict.tri),
    produced.chem.facility = as.numeric(produced.chem.facility),
    imported.chem.facility = as.numeric(imported.chem.facility),
    pi.chem.facility = as.numeric(pi.chem.facility),
    chemical.intermediate.uses = as.numeric(chemical.intermediate.uses),
    chemical.formulation.component = as.numeric(chemical.formulation.component),
    chemical.article.component = as.numeric(chemical.article.component),
    chemical.manufacturing.aid = as.numeric(chemical.manufacturing.aid),
    chemical.ancilliary.use = as.numeric(chemical.ancilliary.use)
  ) %>%
  select(-pfas.chems)
glimpse(triQc)
#======================================================================================================================#
### Facility-level aggregation---Onsite
#======================================================================================================================#
# library(collapse)
# triQf <- triQc %>%
#   collap(
#     X = .,
#     by = ~
#       year +
#         # facility.id +
#         facility.zipcode +
#         facility.city +
#         fips_code +
#         facility.county +
#         facility.state +
#         state +
#         lat +
#         long +
#         zip.length +
#         naics.code +
#         industry.name +
#         chemical.id +
#         chemical.name +
#         chemical.classification +
#         unit.of.measure +
#         total.fug.air.emissions.onsite +
#         total.point.air.emissions.onsite +
#         total.air.emissions.onsite +
#         total.num.receiving.streams.onsite +
#         total.surface.water.discharge.onsite +
#         total.underground.injection.I.wells.onsite +
#         total.underground.injection.I.IV.wells.onsite +
#         total.underground.injection.onsite +
#         total.landfills.onsite +
#         total.releases.toland.treatment.onsite +
#         total.surface.impoundment.onsite +
#         total.land.releases.other.onsite +
#         total.land.releases.onsite +
#         total.releases.onsite +
#         energy.recovery.onsite +
#         industrial.kiln.onsite +
#         industrial.furnace.onsite +
#         industrial.boiler.onsite +
#         recycling.onsite +
#         metal.recovery.onsite +
#         solvent.recovery.onsite +
#         reuse.onsite +
#         biological.treatment.onsite +
#         chemical.treatment.onsite +
#         incineration.thermal.treatment.onsite +
#         physical.treatment.onsite +
#         material.subandmod +
#         produced.chem.facility +
#         imported.chem.facility +
#         pi.chem.facility +
#         treatment.onsite +
#         air.emissions.treatment.onsite +
#         total.waste.management.onsite +
#         sub.fuel.matsubmod +
#         sub.organic.solvent.matsubmod +
#         sub.rawm.feedstock.reactchem.matsubmod +
#         sub.manu.proccess.ancilliary.chems.matsubmod +
#         mod.content.grade.purity.chems.matsubmod +
#         other.matmods.matsubmod +
#         product.modification +
#         devd.newproductline.pmod +
#         alt.dim.comp.design.pmod +
#         mod.packaging.pmod +
#         other.pmods.pmod +
#         process.equip.modification +
#         optimised.process.efficiency.pequipmod +
#         recirculationinprocess.pequipmod +
#         newtech.technique.process.pequipmod +
#         equipment.upgrade.update.pequipmod +
#         other.pequipmods.pequipmod +
#         inventory.material.mgt +
#         better.labelling.testing.immgt +
#         containers.sizechange.immgt +
#         improved.materialhandling.operations.immgt +
#         improved.monitoring.immgt +
#         other.immgts.immgt +
#         operating.practices.training +
#         improved.schdule.operation.procedures.opt +
#         changed.production.schedule.opt +
#         intro.inline.productquality.process.analysis.opt +
#         maxnum.chem.onsite +
#         trade.secret +
#         sanitised +
#         entire.facility +
#         federal.facility +
#         govt.owned.facility +
#         elemental.metal.included +
#         clean.air.act.chems +
#         carcinogenic.chems +
#         metal.restrict.tri +
#         production.ratio.activity.index +
#         chemical.intermediate.uses +
#         chemical.formulation.component +
#         chemical.article.component +
#         chemical.manufacturing.aid +
#         chemical.ancilliary.use +
#         cpi +
#         personal_income +
#         gdp +
#         compensation_to_employees +
#         bea_unit +
#         regional_price_parity +
#         bea_rpp_unit +
#         own_code +
#         annual_avg_estabs +
#         annual_avg_emplvl +
#         total_annual_wages +
#         taxable_annual_wages +
#         annual_contributions +
#         annual_avg_wkly_wage +
#         avg_annual_pay +
#         emp +
#         pay +
#         prode +
#         prodh +
#         prodw +
#         vship +
#         matcost +
#         vadd +
#         invest +
#         invent +
#         energy +
#         cap +
#         equip +
#         plant +
#         tfp4 +
#         tfp5 +
#         population +
#         treated +
#         treated.match +
#         control.match +
#         overlap +
#         state.border.id +
#         treated.cluster.name +
#         control.cluster.name +
#         treated.cluster.id +
#         control.cluster.id +
#         cbcp.id +
#         treated.cluster.population +
#         control.cluster.population +
#         treated.cluster.lat +
#         treated.cluster.long +
#         control.cluster.lat +
#         control.cluster.long +
#         ch.year +
#         ch.amt +
#         sum2.sub.mw.ch +
#         start.mw +
#         end.mw +
#         match.ch.amt +
#         match.ch.year +
#         dist.to.border
#     ,
#     na.rm = T,
#     FUN = fsum,
#     catFUN = fmode,
#     keep.col.order = T,
#     return = "long"
#   ) %>%
#   select(-c(Function, facility.id))
#
# triQf <- triQf[complete.cases(triQf),]
# sum(is.na(triQf))
# sort(unique(triQf$year))
# sort(unique(triQf$state))
# sort(unique(triQf$chemical.name))
# n_distinct(triQf$state)
# n_distinct(triQf$facility.id)
# n_distinct(triQf$facility.zipcode)
# n_distinct(triQf$facility.city)
# n_distinct(triQf$fips_code)
# n_distinct(triQf$facility.county)
# n_distinct(triQf$chemical.name)
# n_distinct(triQf$naics.code)
# n_distinct(triQf$industry.name)
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
        naics.code +
        industry.name +
        chemical.id +
        chemical.name +
        chemical.classification +
        unit.of.measure +
        total.fug.air.emissions.onsite +
        total.point.air.emissions.onsite +
        total.air.emissions.onsite +
        total.num.receiving.streams.onsite +
        total.surface.water.discharge.onsite +
        total.underground.injection.I.wells.onsite +
        total.underground.injection.I.IV.wells.onsite +
        total.underground.injection.onsite +
        total.landfills.onsite +
        total.releases.toland.treatment.onsite +
        total.surface.impoundment.onsite +
        total.land.releases.other.onsite +
        total.land.releases.onsite +
        total.releases.onsite +
        energy.recovery.onsite +
        industrial.kiln.onsite +
        industrial.furnace.onsite +
        industrial.boiler.onsite +
        recycling.onsite +
        metal.recovery.onsite +
        solvent.recovery.onsite +
        reuse.onsite +
        biological.treatment.onsite +
        chemical.treatment.onsite +
        incineration.thermal.treatment.onsite +
        physical.treatment.onsite +
        material.subandmod +
        produced.chem.facility +
        imported.chem.facility +
        pi.chem.facility +
        treatment.onsite +
        air.emissions.treatment.onsite +
        total.waste.management.onsite +
        sub.fuel.matsubmod +
        sub.organic.solvent.matsubmod +
        sub.rawm.feedstock.reactchem.matsubmod +
        sub.manu.proccess.ancilliary.chems.matsubmod +
        mod.content.grade.purity.chems.matsubmod +
        other.matmods.matsubmod +
        product.modification +
        devd.newproductline.pmod +
        alt.dim.comp.design.pmod +
        mod.packaging.pmod +
        other.pmods.pmod +
        process.equip.modification +
        optimised.process.efficiency.pequipmod +
        recirculationinprocess.pequipmod +
        newtech.technique.process.pequipmod +
        equipment.upgrade.update.pequipmod +
        other.pequipmods.pequipmod +
        inventory.material.mgt +
        better.labelling.testing.immgt +
        containers.sizechange.immgt +
        improved.materialhandling.operations.immgt +
        improved.monitoring.immgt +
        other.immgts.immgt +
        operating.practices.training +
        improved.schdule.operation.procedures.opt +
        changed.production.schedule.opt +
        intro.inline.productquality.process.analysis.opt +
        maxnum.chem.onsite +
        trade.secret +
        sanitised +
        entire.facility +
        federal.facility +
        govt.owned.facility +
        elemental.metal.included +
        clean.air.act.chems +
        carcinogenic.chems +
        metal.restrict.tri +
        production.ratio.activity.index +
        chemical.intermediate.uses +
        chemical.formulation.component +
        chemical.article.component +
        chemical.manufacturing.aid +
        chemical.ancilliary.use +
        cpi +
        personal_income +
        gdp +
        compensation_to_employees +
        bea_unit +
        regional_price_parity +
        bea_rpp_unit +
        own_code +
        annual_avg_estabs +
        annual_avg_emplvl +
        total_annual_wages +
        taxable_annual_wages +
        annual_contributions +
        annual_avg_wkly_wage +
        avg_annual_pay +
        emp +
        pay +
        prode +
        prodh +
        prodw +
        vship +
        matcost +
        vadd +
        invest +
        invent +
        energy +
        cap +
        equip +
        plant +
        tfp4 +
        tfp5 +
        population +
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
        dist.to.border
    ,
    na.rm = T,
    FUN = fsum,
    catFUN = fmode,
    keep.col.order = T,
    return = "long"
  ) %>%
  select(
    -c(Function, facility.city:fips_code, lat, long, treated.cluster.name:cbcp.id,
       treated.cluster.lat:control.cluster.long, treated.cluster.population, control.cluster.population)
  ) %>%
  mutate(
    # lat = as.numeric(lat),
    # long = as.numeric(long),
    # treated.cluster.lat = as.numeric(treated.cluster.lat),
    # treated.cluster.long = as.numeric(treated.cluster.long),
    # control.cluster.lat = as.numeric(treated.cluster.lat),
    # control.cluster.long = as.numeric(control.cluster.long),
    industrial.kiln.onsite = as.numeric(industrial.kiln.onsite),
    industrial.furnace.onsite = as.numeric(industrial.furnace.onsite),
    industrial.boiler.onsite = as.numeric(industrial.boiler.onsite),
    metal.recovery.onsite = as.numeric(metal.recovery.onsite),
    solvent.recovery.onsite = as.numeric(solvent.recovery.onsite),
    reuse.onsite = as.numeric(reuse.onsite),
    biological.treatment.onsite = as.numeric(biological.treatment.onsite),
    chemical.treatment.onsite = as.numeric(chemical.treatment.onsite),
    incineration.thermal.treatment.onsite = as.numeric(incineration.thermal.treatment.onsite),
    physical.treatment.onsite = as.numeric(physical.treatment.onsite),
    material.subandmod = as.numeric(material.subandmod),
    sub.fuel.matsubmod = as.numeric(sub.fuel.matsubmod),
    sub.organic.solvent.matsubmod = as.numeric(sub.organic.solvent.matsubmod),
    sub.rawm.feedstock.reactchem.matsubmod = as.numeric(sub.rawm.feedstock.reactchem.matsubmod),
    sub.manu.proccess.ancilliary.chems.matsubmod = as.numeric(sub.manu.proccess.ancilliary.chems.matsubmod),
    mod.content.grade.purity.chems.matsubmod = as.numeric(mod.content.grade.purity.chems.matsubmod),
    other.matmods.matsubmod = as.numeric(other.matmods.matsubmod),
    product.modification = as.numeric(product.modification),
    devd.newproductline.pmod = as.numeric(devd.newproductline.pmod),
    alt.dim.comp.design.pmod = as.numeric(alt.dim.comp.design.pmod),
    mod.packaging.pmod = as.numeric(mod.packaging.pmod),
    other.pmods.pmod = as.numeric(other.pmods.pmod),
    process.equip.modification = as.numeric(process.equip.modification),
    optimised.process.efficiency.pequipmod = as.numeric(optimised.process.efficiency.pequipmod),
    recirculationinprocess.pequipmod = as.numeric(recirculationinprocess.pequipmod),
    newtech.technique.process.pequipmod = as.numeric(newtech.technique.process.pequipmod),
    equipment.upgrade.update.pequipmod = as.numeric(equipment.upgrade.update.pequipmod),
    other.pequipmods.pequipmod = as.numeric(other.pequipmods.pequipmod),
    inventory.material.mgt = as.numeric(inventory.material.mgt),
    better.labelling.testing.immgt = as.numeric(better.labelling.testing.immgt),
    containers.sizechange.immgt = as.numeric(containers.sizechange.immgt),
    improved.materialhandling.operations.immgt = as.numeric(improved.materialhandling.operations.immgt),
    improved.monitoring.immgt = as.numeric(improved.monitoring.immgt),
    other.immgts.immgt = as.numeric(other.immgts.immgt),
    operating.practices.training = as.numeric(operating.practices.training),
    improved.schdule.operation.procedures.opt = as.numeric(improved.schdule.operation.procedures.opt),
    changed.production.schedule.opt = as.numeric(changed.production.schedule.opt),
    intro.inline.productquality.process.analysis.opt = as.numeric(intro.inline.productquality.process.analysis.opt),
    trade.secret = as.numeric(trade.secret),
    sanitised = as.numeric(sanitised),
    entire.facility = as.numeric(entire.facility),
    federal.facility = as.numeric(federal.facility),
    govt.owned.facility = as.numeric(govt.owned.facility),
    elemental.metal.included = as.numeric(elemental.metal.included),
    clean.air.act.chems = as.numeric(clean.air.act.chems),
    carcinogenic.chems = as.numeric(carcinogenic.chems),
    metal.restrict.tri = as.numeric(metal.restrict.tri),
    produced.chem.facility = as.numeric(produced.chem.facility),
    imported.chem.facility = as.numeric(imported.chem.facility),
    pi.chem.facility = as.numeric(pi.chem.facility),
    chemical.intermediate.uses = as.numeric(chemical.intermediate.uses),
    chemical.formulation.component = as.numeric(chemical.formulation.component),
    chemical.article.component = as.numeric(chemical.article.component),
    chemical.manufacturing.aid = as.numeric(chemical.manufacturing.aid),
    chemical.ancilliary.use = as.numeric(chemical.ancilliary.use)
  )

sum(is.na(triQs))
sort(unique(triQs$year))
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
### Standardising variable formats
#======================================================================================================================#
sum_up(triQc, c(vadd, matcost, prodh, energy))
sum_up(triQs, c(vadd, matcost, prodh, energy))
# triQf <- triQf %>%
#   mutate(
#     emp = emp * 1000,
#     prode = prode * 1000,
#     pay = pay * 1000000,
#     prodh = prodh * 1000000,
#     prodw = prodw * 1000000,
#     vship = vship * 1000000,
#     matcost = matcost * 1000000,
#     vadd = vadd * 1000000,
#     invest = invest * 1000000,
#     invent = invent * 1000000,
#     energy = energy * 1000000,
#     cap = cap * 1000000,
#     equip = equip * 1000000,
#     plant = plant * 1000000
#   )
#======================================================================================================================#
### Variables Creation
### Log Transformations with x + 1 to correct for 0
#======================================================================================================================#
# If $1 bill weighs 1 gram; and 454 grams = 1lb. Then, $454 bills = 1lb. Thus, ind.output.lb = ind.output / 454
triQc <- triQc %>%
  mutate(
    # ind.output = vadd + prodh + matcost + energy,
    ind.output.lb = vadd / 454,
    output.perworker = vadd / emp,
    l.output.perworker = log(x = output.perworker),
    output.perhr = vadd / prodh,
    l.output.perhr = log(x = output.perhr),
    wage.perworker = prodw / prode,
    l.wage.perworker = log(x = wage.perworker),
    wage.perhr = prodw / prodh,
    l.wage.perhr = log(x = wage.perhr),
    energy.intensity = energy / vadd,
    l.energy.intensity = log(x = energy.intensity),
    total.air.emissions.onsite.intensity = total.air.emissions.onsite / ind.output.lb,
    l.total.air.emissions.onsite.intensity = log(x = (total.air.emissions.onsite.intensity + 1)),
    total.fug.air.emissions.onsite.intensity = total.fug.air.emissions.onsite / ind.output.lb,
    l.total.fug.air.emissions.onsite.intensity = log(x = (total.fug.air.emissions.onsite.intensity + 1)),
    total.point.air.emissions.onsite.intensity = total.point.air.emissions.onsite / ind.output.lb,
    l.total.point.air.emissions.onsite.intensity = log(x = (total.point.air.emissions.onsite.intensity + 1)),
    total.num.receiving.streams.onsite.intensity = total.num.receiving.streams.onsite / ind.output.lb,
    l.total.num.receiving.streams.onsite.intensity = log(x = (total.num.receiving.streams.onsite.intensity + 1)),
    total.surface.water.discharge.onsite.intensity = total.surface.water.discharge.onsite / ind.output.lb,
    l.total.surface.water.discharge.onsite.intensity = log(x = (total.surface.water.discharge.onsite.intensity + 1)),
    total.underground.injection.I.wells.onsite.intensity = total.underground.injection.I.wells.onsite / ind.output.lb,
    l.total.underground.injection.I.wells.onsite.intensity = log(x = (total.underground.injection.I.wells.onsite.intensity + 1)),
    total.underground.injection.I.IV.wells.onsite.intensity = total.underground.injection.I.IV.wells.onsite / ind.output.lb,
    l.total.underground.injection.I.IV.wells.onsite.intensity = log(x = (total.underground.injection.I.IV.wells.onsite.intensity + 1)),
    total.underground.injection.onsite.intensity = total.underground.injection.onsite / ind.output.lb,
    l.total.underground.injection.onsite.intensity = log(x = (total.underground.injection.onsite.intensity + 1)),
    total.landfills.onsite.intensity = total.landfills.onsite / ind.output.lb,
    l.total.landfills.onsite.intensity = log(x = (total.landfills.onsite.intensity + 1)),
    total.releases.toland.treatment.onsite.intensity = total.releases.toland.treatment.onsite / ind.output.lb,
    l.total.releases.toland.treatment.onsite.intensity = log(x = (total.releases.toland.treatment.onsite.intensity + 1)),
    l.total.surface.impoundment.onsite.intensity = log((total.surface.impoundment.onsite / ind.output.lb) + 1),
    total.surface.impoundment.onsite.intensity = total.surface.impoundment.onsite / ind.output.lb,
    l.total.land.releases.other.onsite.intensity = log(x = (total.surface.impoundment.onsite.intensity + 1)),
    total.land.releases.onsite.intensity = total.land.releases.onsite / ind.output.lb,
    l.total.land.releases.onsite.intensity = log(x = (total.land.releases.onsite.intensity + 1)),
    total.releases.onsite.intensity = total.releases.onsite / ind.output.lb,
    l.total.releases.onsite.intensity = log(x = (total.releases.onsite.intensity + 1)),
    l.annual.avg.emplvl = log(x = (annual_avg_emplvl + 1)),
    l.total.annual.wages = log(x = (total_annual_wages + 1)),
    l.taxable.annual.wages = log(x = (taxable_annual_wages + 1)),
    l.annual.contributions = log(x = (annual_contributions + 1)),
    l.annual.avg.wkly.wages = log(x = (annual_avg_wkly_wage + 1)),
    l.avg.annual.pay = log(x = (avg_annual_pay + 1)),
    l.cpi = log(x = (cpi)),
    private.naics = ifelse(test = own_code == 5, yes = 1, no = 0),
    annual.avg.emplvl.1 = stats::lag(annual_avg_emplvl, k = 1),
    total.annual.wages.1 = stats::lag(total_annual_wages, k = 1),
    taxable.annual.wages.1 = stats::lag(taxable_annual_wages, k = 1),
    annual.contributions.1 = stats::lag(annual_contributions, k = 1),
    annual.avg.wkly.wages.1 = stats::lag(annual_avg_wkly_wage, k = 1),
    avg.annual.pay.1 = stats::lag(avg_annual_pay, k = 1),
    cpi.1 = stats::lag(cpi, k = 1),
    emp.1 = stats::lag(emp, k = 1),
    l.emp = log(emp),
    l.pay = log(pay),
    l.prode = log(prode),
    l.prodh = log(prodh),
    l.prodw = log(prodw),
    l.revenue = log(vship),
    l.matcost = log(matcost),
    l.invest = log(invest),
    l.invent = log(invent),
    l.vadd = log(vadd),
    l.revenue = log(vship),
    l.cap = log(cap),
    l.equip = log(equip),
    l.plant = log(plant),
    l.tfp4 = log(tfp4),
    l.tfp5 = log(tfp5),
    gdp.pc = gdp / population,
    gdppc.1 = stats::lag(gdp.pc, k = 1),
    gdp.1 = stats::lag(gdp, k = 1),
    pinc.1 = stats::lag(personal_income, k = 1),
    annual.avg.estabs.1 = stats::lag(annual_avg_estabs, k = 1),
    population.1 = stats::lag(population, k = 1),
    cap.1 = stats::lag(cap, k = 1),
    equip.1 = stats::lag(equip, k = 1),
    plant.1 = stats::lag(plant, k = 1),
    vadd.1 = stats::lag(vadd, k = 1),
    revenue.1 = stats::lag(vship, k = 1),
    invest.1 = stats::lag(invest, k = 1),
    invent.1 = stats::lag(invent, k = 1),
  )
sum_up(triQc, c(vadd, total.point.air.emissions.onsite.intensity,
                total.fug.air.emissions.onsite.intensity, total.air.emissions.onsite.intensity,
                total.surface.impoundment.onsite.intensity, total.surface.water.discharge.onsite))

triQs <- triQs %>%
  mutate(
    # ind.output = vadd + prodh + matcost + energy,
    ind.output.lb = vadd / 454,
    output.perworker = vadd / emp,
    l.output.perworker = log(x = output.perworker),
    output.perhr = vadd / prodh,
    l.output.perhr = log(x = output.perhr),
    wage.perworker = prodw / prode,
    l.wage.perworker = log(x = wage.perworker),
    wage.perhr = prodw / prodh,
    l.wage.perhr = log(x = wage.perhr),
    energy.intensity = energy / vadd,
    l.energy.intensity = log(x = energy.intensity),
    total.air.emissions.onsite.intensity = total.air.emissions.onsite / ind.output.lb,
    l.total.air.emissions.onsite.intensity = log(x = (total.air.emissions.onsite.intensity + 1)),
    total.fug.air.emissions.onsite.intensity = total.fug.air.emissions.onsite / ind.output.lb,
    l.total.fug.air.emissions.onsite.intensity = log(x = (total.fug.air.emissions.onsite.intensity + 1)),
    total.point.air.emissions.onsite.intensity = total.point.air.emissions.onsite / ind.output.lb,
    l.total.point.air.emissions.onsite.intensity = log(x = (total.point.air.emissions.onsite.intensity + 1)),
    total.surface.water.discharge.onsite.intensity = total.surface.water.discharge.onsite / ind.output.lb,
    l.total.surface.water.discharge.onsite.intensity = log(x = (total.surface.water.discharge.onsite.intensity + 1)),
    total.underground.injection.I.wells.onsite.intensity = total.underground.injection.I.wells.onsite / ind.output.lb,
    l.total.underground.injection.I.wells.onsite.intensity = log(x = (total.underground.injection.I.wells.onsite.intensity + 1)),
    total.underground.injection.I.IV.wells.onsite.intensity = total.underground.injection.I.IV.wells.onsite / ind.output.lb,
    l.total.underground.injection.I.IV.wells.onsite.intensity = log(x = (total.underground.injection.I.IV.wells.onsite.intensity + 1)),
    total.landfills.onsite.intensity = total.landfills.onsite / ind.output.lb,
    l.total.landfills.onsite.intensity = log(x = (total.landfills.onsite.intensity + 1)),
    total.releases.toland.treatment.onsite.intensity = total.releases.toland.treatment.onsite / ind.output.lb,
    l.total.releases.toland.treatment.onsite.intensity = log(x = (total.releases.toland.treatment.onsite.intensity + 1)),
    l.total.surface.impoundment.onsite.intensity = log((total.surface.impoundment.onsite / ind.output.lb) + 1),
    total.surface.impoundment.onsite.intensity = total.surface.impoundment.onsite / ind.output.lb,
    l.total.land.releases.other.onsite.intensity = log(x = (total.surface.impoundment.onsite.intensity + 1)),
    total.land.releases.onsite.intensity = total.land.releases.onsite / ind.output.lb,
    l.total.land.releases.onsite.intensity = log(x = (total.land.releases.onsite.intensity + 1)),
    total.releases.onsite.intensity = total.releases.onsite / ind.output.lb,
    l.total.releases.onsite.intensity = log(x = (total.releases.onsite.intensity + 1)),
    l.annual.avg.emplvl = log(x = (annual_avg_emplvl + 1)),
    l.total.annual.wages = log(x = (total_annual_wages + 1)),
    l.taxable.annual.wages = log(x = (taxable_annual_wages + 1)),
    l.annual.contributions = log(x = (annual_contributions + 1)),
    l.annual.avg.wkly.wages = log(x = (annual_avg_wkly_wage + 1)),
    l.avg.annual.pay = log(x = (avg_annual_pay + 1)),
    l.cpi = log(x = (cpi)),
    private.naics = ifelse(test = own_code == 5, yes = 1, no = 0),
    annual.avg.emplvl.1 = stats::lag(annual_avg_emplvl, k = 1),
    total.annual.wages.1 = stats::lag(total_annual_wages, k = 1),
    taxable.annual.wages.1 = stats::lag(taxable_annual_wages, k = 1),
    annual.contributions.1 = stats::lag(annual_contributions, k = 1),
    annual.avg.wkly.wages.1 = stats::lag(annual_avg_wkly_wage, k = 1),
    avg.annual.pay.1 = stats::lag(avg_annual_pay, k = 1),
    cpi.1 = stats::lag(cpi, k = 1),
    emp.1 = stats::lag(emp, k = 1),
    l.emp = log(emp),
    l.pay = log(pay),
    l.prode = log(prode),
    l.prodh = log(prodh),
    l.prodw = log(prodw),
    l.revenue = log(vship),
    l.matcost = log(matcost),
    l.invest = log(invest),
    l.invent = log(invent),
    l.vadd = log(vadd),
    l.revenue = log(vship),
    l.cap = log(cap),
    l.equip = log(equip),
    l.plant = log(plant),
    l.tfp4 = log(tfp4),
    l.tfp5 = log(tfp5),
    gdp.pc = gdp / population,
    gdppc.1 = stats::lag(gdp.pc, k = 1),
    gdp.1 = stats::lag(gdp, k = 1),
    pinc.1 = stats::lag(personal_income, k = 1),
    annual.avg.estabs.1 = stats::lag(annual_avg_estabs, k = 1),
    population.1 = stats::lag(population, k = 1),
    cap.1 = stats::lag(cap, k = 1),
    equip.1 = stats::lag(equip, k = 1),
    plant.1 = stats::lag(plant, k = 1),
    vadd.1 = stats::lag(vadd, k = 1),
    revenue.1 = stats::lag(vship, k = 1),
    invest.1 = stats::lag(invest, k = 1),
    invent.1 = stats::lag(invent, k = 1),
  )
sum_up(triQs, c(vadd, total.point.air.emissions.onsite.intensity,
                total.fug.air.emissions.onsite.intensity, total.air.emissions.onsite.intensity,
                total.surface.impoundment.onsite.intensity, total.surface.water.discharge.onsite,))
#======================================================================================================================#
### Experiment Design
#======================================================================================================================#
sort(unique(triQc$industry.name))

triQc <- triQc %>%
  rename(fips.code = fips_code) %>%
  mutate(
    hap.chems = case_when(chemical.classification == "TRI" ~ 1, T ~ 0),
    dioxin.chems = case_when(chemical.classification == "Dioxin" ~ 1, T ~ 0),
    pbt.chems = case_when(chemical.classification == "PBT" ~ 1, T ~ 0),
  ) %>%
  mutate(
    e.treated = case_when(year >= ch.year ~ 1, T ~ 0), #states e-years away from the initial treatment year
    post = case_when(year == 2014 | year == 2015 | year == 2017 ~ 1, T ~ 0),
    rel.year = year - ch.year + 2014,
    facility.id = as.numeric(facility.id),
    facility.zipcode = as.numeric(facility.zipcode),
    naics.code = as.numeric(naics.code),
    fips.code = as.numeric(fips.code),
    facility.state.id = as.numeric(as.factor(facility.state)),
    treated.cluster.id = as.numeric(treated.cluster.id),
    control.cluster.id = as.numeric(control.cluster.id),
    fips.state.fe = as.numeric(fips.code) * as.numeric(facility.state.id),
    zip.fips.fe = as.numeric(facility.zipcode) * as.numeric(fips.code),
    fac.chem.fe = as.numeric(facility.id) * as.numeric(as.factor(chemical.id)),
    facility.year.fe = as.numeric(facility.id) * year,
    chemical.year.fe = as.numeric(as.factor(chemical.id)) * year,
    zip.year.fe = as.numeric(facility.zipcode) * year,
    fips.year.fe = as.numeric(fips.code) * year,
    state.year.fe = as.numeric(facility.state.id) * year,
    treated.cluster.year.fe = as.numeric(treated.cluster.id) * year,
    control.cluster.year.fe = as.numeric(control.cluster.id) * year
  )

triQs <- triQs %>%
  mutate(
    hap.chems = case_when(chemical.classification == "TRI" ~ 1, T ~ 0),
    dioxin.chems = case_when(chemical.classification == "Dioxin" ~ 1, T ~ 0),
    pbt.chems = case_when(chemical.classification == "PBT" ~ 1, T ~ 0),
  ) %>%
  mutate(
    e.treated = case_when(year >= ch.year ~ 1, T ~ 0), #states e-years away from the initial treatment year
    post = case_when(year == 2014 | year == 2015 | year == 2017 ~ 1, T ~ 0),
    rel.year = year - ch.year + 2014,
    facility.id = as.numeric(facility.id),
    facility.zipcode = as.numeric(facility.zipcode),
    naics.code = as.numeric(naics.code),
    facility.state.id = as.numeric(as.factor(facility.state)),
    treated.match.fe = as.numeric(as.factor(treated.match)),
    control.match.fe = as.numeric(as.factor(control.match)),
    fac.chem.fe = as.numeric(facility.id) * as.numeric(as.factor(chemical.id)),
    facility.year.fe = as.numeric(facility.id) * year,
    chemical.year.fe = as.numeric(as.factor(chemical.id)) * year,
    zip.year.fe = as.numeric(facility.zipcode) * year,
    state.year.fe = as.numeric(facility.state.id) * year,
    treated.match.year.fe = as.numeric(as.factor(treated.match)) * year,
    control.match.year.fe = as.numeric(as.factor(control.match)) * year
  )
#======================================================================================================================#
### Save data
#======================================================================================================================#
start_time <- Sys.time()
write_rds(triQc, file = "./Data_PhD/US/BLS/onsite/triQc_on.rds", compress = "xz")
write_rds(triQs, file = "./Data_PhD/US/BLS/onsite/triQs_on.rds", compress = "xz")
end_time <- Sys.time()
end_time - start_time
#======================================================================================================================#