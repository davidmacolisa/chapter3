#======================================================================================================================#
library(tidyverse)
library(statar)
library(fixest)
library(did)
library(car)
#======================================================================================================================#
## Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading Data
#======================================================================================================================#
file <- "./Data_PhD/US/BLS/offsite/triQc_off.rds"
triQc <- read_rds(file = file)
#======================================================================================================================#
### Labour Cost---Wage per hr, weekly wages, and  total wages
#======================================================================================================================#
table(triQc$facility.state, triQc$ch.year)
n_distinct(triQc$chemical.name)
sort(unique(triQc$chemical.name))
sort(unique(triQc$offsite.zipcode))
sort(unique(triQc$facility.state.id))
sort(unique(triQc$year))
sort(unique(triQc$rel.year))
#======================================================================================================================#
### Offsite: Total releases
#======================================================================================================================#
did_releases <- fixest::feols(
  l.total.releases.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    federal.facility +
    produced.chem.facility +
    imported.chem.facility +
    chemical.formulation.component +
    chemical.article.component +
    chemical.manufacturing.aid +
    chemical.ancilliary.use +
    production.ratio.activity.index +
    maxnum.chem.onsite +
    clean.air.act.chems +
    hap.chems +
    pbt.chems
    |
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(did_releases, digits = 3, digits.stats = 3)
etable(did_releases, agg = "ATT", digits = 3, digits.stats = 3)
etable(did_releases, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_releases, xlim = c(-3, 3), ylim = c(-0.1, 0.5), col = "blue",
              main = "Toal Offsite Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total releases, unknown
#======================================================================================================================#
did_releases_unknown <- fixest::feols(
  l.total.releases.unknown.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    federal.facility +
    produced.chem.facility +
    imported.chem.facility +
    chemical.formulation.component +
    chemical.article.component +
    chemical.manufacturing.aid +
    chemical.ancilliary.use +
    production.ratio.activity.index +
    maxnum.chem.onsite +
    clean.air.act.chems +
    hap.chems +
    pbt.chems
    |
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_releases_unknown, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_releases_unknown, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_releases_unknown, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Offsite Releases Intensity, Unknown", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total releases, waste broker
#======================================================================================================================#
did_releases_wastebroker <- fixest::feols(
  l.total.releases.wastebroker.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    federal.facility +
    produced.chem.facility +
    imported.chem.facility +
    chemical.formulation.component +
    chemical.article.component +
    chemical.manufacturing.aid +
    chemical.ancilliary.use +
    production.ratio.activity.index +
    maxnum.chem.onsite +
    clean.air.act.chems +
    hap.chems +
    pbt.chems
    |
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_releases_wastebroker, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_releases_wastebroker, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_releases_wastebroker, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Offsite Releases Intensity, Wastebroker", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total releases, others
#======================================================================================================================#
did_releases_others <- fixest::feols(
  l.total.releases.other.mgt.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    federal.facility +
    produced.chem.facility +
    imported.chem.facility +
    chemical.formulation.component +
    chemical.article.component +
    chemical.manufacturing.aid +
    chemical.ancilliary.use +
    production.ratio.activity.index +
    maxnum.chem.onsite +
    clean.air.act.chems +
    hap.chems +
    pbt.chems
    |
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_releases_others, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_releases_others, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_releases_others, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Offsite Releases Intensity, Others", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total releases, metal solidification
#======================================================================================================================#
did_releases_metalsolidify <- fixest::feols(
  l.total.releases.metalsolidify.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    federal.facility +
    produced.chem.facility +
    imported.chem.facility +
    chemical.formulation.component +
    chemical.article.component +
    chemical.manufacturing.aid +
    chemical.ancilliary.use +
    production.ratio.activity.index +
    maxnum.chem.onsite +
    clean.air.act.chems +
    hap.chems +
    pbt.chems
    |
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_releases_metalsolidify, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_releases_metalsolidify, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_releases_metalsolidify, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
              main = "Total Offsite Releases Intensity, Metal Solidification", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total releases, storage
#======================================================================================================================#
did_releases_storage <- fixest::feols(
  l.total.releases.storage.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    federal.facility +
    produced.chem.facility +
    imported.chem.facility +
    chemical.formulation.component +
    chemical.article.component +
    chemical.manufacturing.aid +
    chemical.ancilliary.use +
    production.ratio.activity.index +
    maxnum.chem.onsite +
    clean.air.act.chems +
    hap.chems +
    pbt.chems
    |
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_releases_storage, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_releases_storage, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_releases_storage, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Offsite Releases Intensity, Storage", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_releases_offsite.pdf", width = 12, height = 7)
par(mfrow = c(2, 3))
fixest::iplot(did_releases, xlim = c(-3, 3), ylim = c(-0.2, 0.6), col = "blue",
              main = "Total Offsite Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_releases_unknown, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Offsite Releases Intensity, Unknown", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_releases_wastebroker, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Offsite Releases Intensity, Waste Broker", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_releases_others, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Offsite Releases Intensity, Others", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_releases_metalsolidify, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Offsite Releases Intensity, Metal Solidification", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_releases_storage, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Offsite Releases Intensity, Storage", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Offsite: Total land releases
#======================================================================================================================#
did_land_releases <- fixest::feols(
  l.total.land.releases.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    federal.facility +
    produced.chem.facility +
    imported.chem.facility +
    chemical.formulation.component +
    chemical.article.component +
    chemical.manufacturing.aid +
    chemical.ancilliary.use +
    production.ratio.activity.index +
    maxnum.chem.onsite +
    clean.air.act.chems +
    hap.chems +
    pbt.chems
    |
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_land_releases, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_land_releases, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_land_releases, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Land Releases Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total land releases, others
#======================================================================================================================#
did_land_releases_others <- fixest::feols(
  l.total.land.releases.other.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    federal.facility +
    produced.chem.facility +
    imported.chem.facility +
    chemical.formulation.component +
    chemical.article.component +
    chemical.manufacturing.aid +
    chemical.ancilliary.use +
    production.ratio.activity.index +
    maxnum.chem.onsite +
    clean.air.act.chems +
    hap.chems +
    pbt.chems
    |
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_land_releases_others, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_land_releases_others, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_land_releases_others, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Offsite Land Releases Intensity, Others", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total waste water releases
#======================================================================================================================#
did_waste_water_releases <- fixest::feols(
  l.total.wastewater.releases.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    federal.facility +
    produced.chem.facility +
    imported.chem.facility +
    chemical.formulation.component +
    chemical.article.component +
    chemical.manufacturing.aid +
    chemical.ancilliary.use +
    production.ratio.activity.index +
    maxnum.chem.onsite +
    clean.air.act.chems +
    hap.chems +
    pbt.chems
    |
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_waste_water_releases, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_waste_water_releases, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_waste_water_releases, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Waste Water Releases Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total surface impoundment
#======================================================================================================================#
did_surface_impoundment <- fixest::feols(
  l.total.surface.impoundment.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    federal.facility +
    produced.chem.facility +
    imported.chem.facility +
    chemical.formulation.component +
    chemical.article.component +
    chemical.manufacturing.aid +
    chemical.ancilliary.use +
    production.ratio.activity.index +
    maxnum.chem.onsite +
    clean.air.act.chems +
    hap.chems +
    pbt.chems
    |
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_surface_impoundment, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_surface_impoundment, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_surface_impoundment, xlim = c(-3, 3), ylim = c(-0.01, 0.02), col = "blue",
              main = "Total Surface Impoundment Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total toland treatment
#======================================================================================================================#
did_toland_treatment <- fixest::feols(
  l.total.releases.toland.treatment.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    federal.facility +
    produced.chem.facility +
    imported.chem.facility +
    chemical.formulation.component +
    chemical.article.component +
    chemical.manufacturing.aid +
    chemical.ancilliary.use +
    production.ratio.activity.index +
    maxnum.chem.onsite +
    clean.air.act.chems +
    hap.chems +
    pbt.chems
    |
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_toland_treatment, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_toland_treatment, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_toland_treatment, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total ToLand Treatment Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total landfills
#======================================================================================================================#
did_landfills <- fixest::feols(
  l.total.landfills.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    federal.facility +
    produced.chem.facility +
    imported.chem.facility +
    chemical.formulation.component +
    chemical.article.component +
    chemical.manufacturing.aid +
    chemical.ancilliary.use +
    production.ratio.activity.index +
    maxnum.chem.onsite +
    clean.air.act.chems +
    hap.chems +
    pbt.chems
    |
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_landfills, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_landfills, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_landfills, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
              main = "Total Landfills Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total Underground injection
#======================================================================================================================#
did_underground_injection <- fixest::feols(
  l.total.underground.injection.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    federal.facility +
    produced.chem.facility +
    imported.chem.facility +
    chemical.formulation.component +
    chemical.article.component +
    chemical.manufacturing.aid +
    chemical.ancilliary.use +
    production.ratio.activity.index +
    maxnum.chem.onsite +
    clean.air.act.chems +
    hap.chems +
    pbt.chems
    |
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_underground_injection, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_underground_injection, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_underground_injection, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
              main = "Total Underground Injection Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_land_releases_offsite.pdf", width = 12, height = 10)
par(mfrow = c(3, 3))
fixest::iplot(did_land_releases, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Land Releases Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_others, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Offsite Land Releases Intensity, Others", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_waste_water_releases, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Waste Water Releases Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment, xlim = c(-3, 3), ylim = c(-0.01, 0.02), col = "blue",
              main = "Total Surface Impoundment Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_toland_treatment, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total ToLand Treatment Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_landfills, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
              main = "Total Landfills Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_underground_injection, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
              main = "Total Underground Injection Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#