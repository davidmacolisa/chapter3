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
  group_by(facility.state, facility.county, year) %>%
  filter(potw.zip.length == 5) %>%
  mutate(
    tot.ch.amt = ch.amt + sum2.sub.mw.ch,
    end.mw = start.mw + tot.ch.amt
  ) %>%
  select(c(
    year, facility.id, facility.zipcode, facility.city, facility.county, fips_code, facility.state, state,
    lat, long, potw.id, potw.zipcode, potw.city, potw.county, naics.code:unit.of.measure,
    potw.state, potw.zip.length, contains(match = "potw"), entire.facility, govt.owned.facility, clean.air.act.chems,
    carcinogenic.chems, metal.restrict.tri, produced.chem.facility, production.ratio.activity.index,
    production.or.activity, imported.chem.facility, pi.chem.facility, chemical.formulation.component,
    chemical.article.component, chemical.manufacturing.aid, chemical.ancilliary.use, comment.type,
    comment.type.description, comment.text, classification, personal_income:dtfp5, population, treated:sum2.sub.mw.ch,
    tot.ch.amt, start.mw, end.mw, match.ch.amt, match.ch.year, dist.to.border
  )) %>%
  data.frame()
end_time <- Sys.time()
end_time - start_time
gc()

triQc <- triQ %>% select(-c(comment.type, comment.type.description, comment.text, classification))
sum_up(triQc, potw.releases.underground.Iwells.offsite:total.potw.management.offsite)
sum(is.na(triQc))
triQc <- triQc[complete.cases(triQc),]
sum_up(triQc, potw.releases.underground.Iwells.offsite:total.potw.management.offsite)

sort(unique(triQc$facility.state))
sort(unique(triQc$potw.state))
n_distinct(triQc$facility.state)
n_distinct(triQc$potw.state)
n_distinct(triQc$treated.match)
n_distinct(triQc$control.match)
n_distinct(triQc$chemical.id)
sum_up(triQc %>% filter(treated == 0), c(ch.amt, sum2.sub.mw.ch, end.mw))

glimpse(triQc)
str(triQc)
#======================================================================================================================#
### For the state-level analysis---Onsite
### Collapse triQc to state level
#======================================================================================================================#
library(collapse)
triQs <- triQc %>%
  collap(
    X = .,
    by = ~
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
        entire.facility +
        govt.owned.facility +
        clean.air.act.chems +
        carcinogenic.chems +
        metal.restrict.tri +
        produced.chem.facility +
        imported.chem.facility +
        pi.chem.facility +
        production.or.activity +
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
    -c(Function, facility.id:fips_code, lat:potw.county, industry.category, potw.zip.length,
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
    # lat = as.numeric(lat),
    # long = as.numeric(long),
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
    output.perworker = (ind.output / emp),
    output.perhr = (ind.output / prodh),
    wage.perhr = prodw / (prodh),
    energy.intensity = (energy / ind.output),

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
write_rds(triQc, file = "./Data_PhD/US/BLS/offsite/triQc_off.rds", compress = "xz")
write_rds(triQs, file = "./Data_PhD/US/BLS/offsite/triQs_off.rds", compress = "xz")
end_time <- Sys.time()
end_time - start_time
#======================================================================================================================#