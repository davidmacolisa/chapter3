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
  group_by(facility.county, year) %>%
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
      personal_income:tfp5, population, treated:sum2.sub.mw.ch, tot.ch.amt, start.mw, end.mw, match.ch.amt,
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

sum(is.na(triQc))
sum_up(triQc %>% filter(treated == 0),
       c(total.fug.air.emissions.onsite, total.point.air.emissions.onsite, total.air.emissions.onsite,
         total.num.receiving.streams.onsite, total.surface.water.discharge.onsite,
         total.underground.injection.I.wells.onsite, total.underground.injection.I.IV.wells.onsite,
         total.underground.injection.onsite, total.landfills.onsite, total.releases.toland.treatment.onsite,
         total.surface.impoundment.onsite, total.land.releases.other.onsite, total.land.releases.onsite))

sort(unique(triQc$year))
sort(unique(triQc$state))
n_distinct(triQc$state)
n_distinct(triQc$facility.id)
n_distinct(triQc$facility.city)
n_distinct(triQc$facility.zipcode)
n_distinct(triQc$facility.county)
n_distinct(triQc$chemical.name)
n_distinct(triQc$industry.name)
n_distinct(triQc$naics.code)

#======================================================================================================================#
### Check if the facility ID column is the same for all years
#======================================================================================================================#
# Get unique facility IDs for each year
unique_facility_ids <- lapply(unique(triQc$year), function(y) unique(triQc$facility.id[triQc$year == y]))

# Check if all unique facility IDs are the same across all years
same_facility_id <- all(sapply(unique_facility_ids, function(ids) identical(ids, unique_facility_ids[[1]])))

# Print the result
if(same_facility_id) {
  print("The facility ID column is the same for all years.")
} else {
  print("The facility ID column is not the same for all years.")
}
#======================================================================================================================#
### For the state-level analysis---Onsite
### Collapse triQc to state level
#======================================================================================================================#
library(collapse)
triQs <- triQc %>%
  collap(
    X = .,
    by = ~facility.id +
      facility.state +
      state +
      lat +
      long +
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
      treated.cluster.lat +
      treated.cluster.long +
      control.cluster.lat +
      control.cluster.long +
      industrial.kiln.onsite +
      industrial.furnace.onsite +
      industrial.boiler.onsite +
      metal.recovery.onsite +
      solvent.recovery.onsite +
      reuse.onsite +
      biological.treatment.onsite +
      chemical.treatment.onsite +
      incineration.thermal.treatment.onsite +
      physical.treatment.onsite +
      material.subandmod +
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
      trade.secret +
      sanitised +
      entire.facility +
      federal.facility +
      govt.owned.facility +
      elemental.metal.included +
      clean.air.act.chems +
      carcinogenic.chems +
      pfas.chems +
      metal.restrict.tri +
      produced.chem.facility +
      imported.chem.facility +
      pi.chem.facility +
      chemical.intermediate.uses +
      chemical.formulation.component +
      chemical.article.component +
      chemical.manufacturing.aid +
      chemical.ancilliary.use +
      bea_unit +
      bea_rpp_unit +
      own_code
    ,
    na.rm = T,
    FUN = fsum,
    catFUN = fmode,
    keep.col.order = T,
    return = "long"
  ) %>%
  select(
    -c(Function, facility.city:facility.county, facility.zipcode:fips_code, zip.length,
       treated.cluster.name:cbcp.id)
  ) %>%
  mutate(
    lat = as.numeric(lat),
    long = as.numeric(long),
    treated.cluster.lat = as.numeric(treated.cluster.lat),
    treated.cluster.long = as.numeric(treated.cluster.long),
    control.cluster.lat = as.numeric(treated.cluster.lat),
    control.cluster.long = as.numeric(control.cluster.long),
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
    pfas.chems = as.numeric(pfas.chems),
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
    pfas.chems = as.numeric(pfas.chems),
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
    l.total.air.emissions.onsite.intensity = log((total.air.emissions.onsite / ind.output) + 1),
    l.total.fug.air.emissions.onsite.intensity = log((total.fug.air.emissions.onsite / ind.output) + 1),
    l.total.point.air.emissions.onsite.intensity = log((total.point.air.emissions.onsite / ind.output) + 1),
    l.total.landfills.onsite.intensity = log((total.landfills.onsite / ind.output) + 1),
    l.total.land.releases.onsite.intensity = log((total.land.releases.onsite / ind.output) + 1),
    l.total.surface.water.discharge.onsite.intensity = log((total.surface.water.discharge.onsite / ind.output) + 1),
    l.total.releases.onsite.intensity = log((total.releases.onsite / ind.output) + 1),
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

triQc <- triQc %>%
  mutate(
    ind.output = vadd + prodh + matcost + energy,
    l.ind.output = log(ind.output),
    l.output.perworker = log((ind.output / emp) + 1),
    l.output.perhr = log((ind.output / prodh) + 1),
    l.wage.perhr = log((prodw / prodh) + 1),
    l.energy.intensity = log((energy / ind.output) + 1),
    l.total.air.emissions.onsite.intensity = log((total.air.emissions.onsite / ind.output) + 1),
    l.total.fug.air.emissions.onsite.intensity = log((total.fug.air.emissions.onsite / ind.output) + 1),
    l.total.point.air.emissions.onsite.intensity = log((total.point.air.emissions.onsite / ind.output) + 1),
    l.total.landfills.onsite.intensity = log((total.landfills.onsite / ind.output) + 1),
    l.total.land.releases.onsite.intensity = log((total.land.releases.onsite / ind.output) + 1),
    l.total.surface.water.discharge.onsite.intensity = log((total.surface.water.discharge.onsite / ind.output) + 1),
    l.total.releases.onsite.intensity = log((total.releases.onsite / ind.output) + 1),
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
# write_rds(triQc, file = "./Data_PhD/US/BLS/onsite/triQc.rds", compress = "xz")
# write_rds(triQs, file = "./Data_PhD/US/BLS/onsite/triQs.rds", compress = "xz")
end_time <- Sys.time()
end_time - start_time
#======================================================================================================================#