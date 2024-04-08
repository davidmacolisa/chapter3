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
triQ <- read_rds(file = "./Data_PhD/US/BLS/onsite/triQc_on.rds") %>%
  group_by(facility.state, year) %>%
  mutate(
    tot.ch.amt = ch.amt + sum2.sub.mw.ch,
    end.mw = start.mw + tot.ch.amt
  ) %>%
  data.frame()
end_time <- Sys.time()
end_time - start_time
gc()

triQc <- triQ %>%
  select(
    -c(comment.type, comment.type.description, comment.text, classification,
       total.release.onsite.catastrophicevents, cap:dtfp5)
  )

sum(is.na(triQc))
triQc <- triQc[complete.cases(triQc),]
sort(unique(triQc$state))
n_distinct(triQc$state)
n_distinct(triQc$chemical.name)
n_distinct(triQc$naics.code)
sum_up(triQc, c(total.fug.air.emissions.onsite, total.point.air.emissions.onsite, total.air.emissions.onsite))
sum_up(triQc %>% filter(treated == 0), c(ch.amt, sum2.sub.mw.ch, end.mw))

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
      treated.cluster.lat +
      treated.cluster.long +
      control.cluster.lat +
      control.cluster.long +
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
      own_code,
    na.rm = T,
    FUN = fsum,
    catFUN = fmode,
    keep.col.order = T,
    return = "long"
  ) %>%
  select(-c(Function, facility.city:facility.county, facility.zipcode:fips_code,
            treated.cluster.name:cbcp.id, treated.cluster.name:cbcp.id)) %>%
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
#======================================================================================================================#
triQc <- triQc %>%
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
    total.releases.onsite.intensity = (total.releases.onsite / ind.output)
  )

triQs <- triQs %>%
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
    total.releases.onsite.intensity = (total.releases.onsite / ind.output)
  )
#======================================================================================================================#
### Save data
#======================================================================================================================#
start_time <- Sys.time()
write_rds(triQc, file = "./Data_PhD/US/BLS/onsite/triQc.rds", compress = "xz")
write_rds(triQs, file = "./Data_PhD/US/BLS/onsite/triQs.rds", compress = "xz")
end_time <- Sys.time()
end_time - start_time
#======================================================================================================================#