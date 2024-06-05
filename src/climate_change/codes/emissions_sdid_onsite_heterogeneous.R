#======================================================================================================================#
### Packages
#======================================================================================================================#
library(tidyverse)
library(statar)
library(fixest)
library(did)
library(TwoWayFEWeights)
library(car)
#======================================================================================================================#
## Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading Data
#======================================================================================================================#
file <- "./Data_PhD/US/BLS/onsite/triQc_on.rds"
triQc <- read_rds(file = file)
#======================================================================================================================#
triQc <- triQc %>%
  mutate(
    highest.emitt.ind = case_when(
      industry.name %in% c("Chemical Manufacturing", "Food Manufacturing",
                           "Leather and Allied Product Manufacturing", "Wood Product Manufacturing") ~ 1, T ~ 0
    )
  )
#======================================================================================================================#
### Triple Differences
#======================================================================================================================#
### Highest Emitting Industries
#======================================================================================================================#
### Onsite: Total releases intensity
#======================================================================================================================#
sdid_total_releases_highemitt <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year):highest.emitt.ind +
    e.treated +
    treated:highest.emitt.ind +
    post:highest.emitt.ind +
    treated +
    highest.emitt.ind +
    post +
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
fixest::etable(sdid_total_releases_highemitt, digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_highemitt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_highemitt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_highemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, HEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_lowemitt <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:highest.emitt.ind +
    treated:highest.emitt.ind +
    post:highest.emitt.ind +
    treated +
    highest.emitt.ind +
    post +
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
fixest::etable(sdid_total_releases_lowemitt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_lowemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, LEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity
#======================================================================================================================#
sdid_air_highemitt <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year):highest.emitt.ind +
    e.treated +
    treated:highest.emitt.ind +
    post:highest.emitt.ind +
    treated +
    highest.emitt.ind +
    post +
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
fixest::etable(sdid_air_highemitt, digits = 3, digits.stats = 3)
fixest::etable(sdid_air_highemitt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_highemitt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_highemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.9), col = "blue",
              main = "Total Air Emissions Intensity, HEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_lowemitt <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:highest.emitt.ind +
    treated:highest.emitt.ind +
    post:highest.emitt.ind +
    treated +
    highest.emitt.ind +
    post +
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
fixest::etable(sdid_air_lowemitt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_lowemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Air Emissions Intensity, LEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity
#======================================================================================================================#
sdid_point_air_highemitt <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year):highest.emitt.ind +
    e.treated +
    treated:highest.emitt.ind +
    post:highest.emitt.ind +
    treated +
    highest.emitt.ind +
    post +
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
fixest::etable(sdid_point_air_highemitt, digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_highemitt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_highemitt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_highemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, HEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_lowemitt <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:highest.emitt.ind +
    treated:highest.emitt.ind +
    post:highest.emitt.ind +
    treated +
    highest.emitt.ind +
    post +
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
fixest::etable(sdid_point_air_lowemitt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_lowemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, LEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity
#======================================================================================================================#
sdid_fug_air_highemitt <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year):highest.emitt.ind +
    e.treated +
    treated:highest.emitt.ind +
    post:highest.emitt.ind +
    treated +
    highest.emitt.ind +
    post +
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
fixest::etable(sdid_fug_air_highemitt, digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_highemitt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_highemitt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_highemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, HEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_lowemitt <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:highest.emitt.ind +
    treated:highest.emitt.ind +
    post:highest.emitt.ind +
    treated +
    highest.emitt.ind +
    post +
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
fixest::etable(sdid_fug_air_lowemitt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_lowemitt, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, LEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity
#======================================================================================================================#
sdid_water_disc_highemitt <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year):highest.emitt.ind +
    e.treated +
    treated:highest.emitt.ind +
    post:highest.emitt.ind +
    treated +
    highest.emitt.ind +
    post +
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
fixest::etable(sdid_water_disc_highemitt, digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_highemitt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_highemitt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_highemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.7), col = "blue",
              main = "Total Surface Water Discharge Intensity, HEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_lowemitt <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:highest.emitt.ind +
    treated:highest.emitt.ind +
    post:highest.emitt.ind +
    treated +
    highest.emitt.ind +
    post +
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
fixest::etable(sdid_water_disc_lowemitt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_lowemitt, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Surface Water Discharge Intensity, LEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Land Releases intensity
#======================================================================================================================#
sdid_land_releases_highemitt <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year):highest.emitt.ind +
    e.treated +
    treated:highest.emitt.ind +
    post:highest.emitt.ind +
    treated +
    highest.emitt.ind +
    post +
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
fixest::etable(sdid_land_releases_highemitt, digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_highemitt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_highemitt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_highemitt, xlim = c(-3, 3), ylim = c(-0.1, 0.3), col = "blue",
              main = "Total Land Releases Intensity, HEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_lowemitt <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:highest.emitt.ind +
    treated:highest.emitt.ind +
    post:highest.emitt.ind +
    treated +
    highest.emitt.ind +
    post +
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
fixest::etable(sdid_land_releases_lowemitt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_lowemitt, xlim = c(-3, 3), ylim = c(-0.2, 0.1), col = "blue",
              main = "Total Land Releases Intensity, LEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Impoundment intensity
#======================================================================================================================#
sdid_surface_impound_highemitt <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year):highest.emitt.ind +
    e.treated +
    treated:highest.emitt.ind +
    post:highest.emitt.ind +
    treated +
    highest.emitt.ind +
    post +
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
fixest::etable(sdid_surface_impound_highemitt, digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impound_highemitt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impound_highemitt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impound_highemitt, xlim = c(-3, 3), ylim = c(-0.05, 0.02), col = "blue",
              main = "Total Surface Impoundment Intensity, HEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impound_lowemitt <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:highest.emitt.ind +
    treated:highest.emitt.ind +
    post:highest.emitt.ind +
    treated +
    highest.emitt.ind +
    post +
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
fixest::etable(sdid_surface_impound_lowemitt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impound_lowemitt, xlim = c(-3, 3), ylim = c(-0.01, 0.08), col = "blue",
              main = "Total Surface Impoundment Intensity, LEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_EMITT.pdf", width = 20, height = 15)
par(mfrow = c(4, 4))
fixest::iplot(sdid_total_releases_highemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, HEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_total_releases_lowemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, LEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_highemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.9), col = "blue",
              main = "Total Air Emissions Intensity, HEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_lowemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Air Emissions Intensity, LEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_highemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, HEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_lowemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, LEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_highemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, HEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_lowemitt, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, LEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_highemitt, xlim = c(-3, 3), ylim = c(-0.5, 0.7), col = "blue",
              main = "Total Surface Water Discharge Intensity, HEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_lowemitt, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Surface Water Discharge Intensity, LEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_lowemitt, xlim = c(-3, 3), ylim = c(-0.2, 0.1), col = "blue",
              main = "Total Land Releases Intensity, LEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_highemitt, xlim = c(-3, 3), ylim = c(-0.1, 0.3), col = "blue",
              main = "Total Land Releases Intensity, HEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impound_highemitt, xlim = c(-3, 3), ylim = c(-0.05, 0.02), col = "blue",
              main = "Total Surface Impoundment Intensity, HEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impound_lowemitt, xlim = c(-3, 3), ylim = c(-0.01, 0.08), col = "blue",
              main = "Total Surface Impoundment Intensity, LEIs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Economic Growth Patterns: GDP
### Higher GDP counties are above the median GDP, and low GDP counties are below the median GDP.
#======================================================================================================================#
# median_gdp <- median(triQc$gdp)
# triQc$high.gdp <- ifelse(triQc$gdp > median_gdp, yes = 1, no = 0)
gdp_50th_percentile <- quantile(triQc[triQc$year == 2013,]$gdp, probs = 0.5) # same as above
triQc$high.gdp <- ifelse(triQc$gdp > gdp_50th_percentile, yes = 1, no = 0)
prop.table(table(triQc$high.gdp)) * 100
#======================================================================================================================#
### Onsite: Total releases intensity #TODO: Run the Sun and Abraham sDID
#======================================================================================================================#
sdid_total_releases_hgdp <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year):high.gdp +
    e.treated +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
fixest::etable(sdid_total_releases_hgdp, digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_hgdp, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_hgdp, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_hgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_lgdp <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:high.gdp +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
fixest::etable(sdid_total_releases_lgdp, digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_lgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity
#======================================================================================================================#
sdid_air_hgdp <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year):high.gdp +
    e.treated +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_hgdp, digits = 3, digits.stats = 3)
fixest::etable(sdid_air_hgdp, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_hgdp, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_hgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Air Emissions Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_lgdp <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:high.gdp +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_lgdp, digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_lgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Air Emissions Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity
#======================================================================================================================#
sdid_point_air_hgdp <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year):high.gdp +
    e.treated +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_hgdp, digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_hgdp, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_hgdp, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_hgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_lgdp <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:high.gdp +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_lgdp, digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_lgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity
#======================================================================================================================#
sdid_fug_air_hgdp <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year):high.gdp +
    e.treated +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_hgdp, digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_hgdp, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_hgdp, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_hgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_lgdp <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:high.gdp +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_lgdp, digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_lgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity
#======================================================================================================================#
sdid_water_disc_hgdp <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year):high.gdp +
    e.treated +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_hgdp, digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_hgdp, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_hgdp, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_hgdp, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Surface Water Discharge Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_lgdp <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:high.gdp +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_lgdp, digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_lgdp, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity
#======================================================================================================================#
sdid_land_releases_hgdp <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year):high.gdp +
    e.treated +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_hgdp, digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_hgdp, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_hgdp, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_hgdp, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Onsite Land Releases Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_lgdp <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:high.gdp +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_lgdp, digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_lgdp, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Onsite Land Releases Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity: Surface impoundment
#======================================================================================================================#
sdid_surface_impound_hgdp <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year):high.gdp +
    e.treated +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impound_hgdp, digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impound_hgdp, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impound_hgdp, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impound_hgdp, xlim = c(-3, 3), ylim = c(-0.02, 0.07), col = "blue",
              main = "Total Surface Impoundment Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impound_lgdp <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:high.gdp +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impound_lgdp, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impound_lgdp, xlim = c(-3, 3), ylim = c(-0.02, 0.07), col = "blue",
              main = "Total Surface Impoundment Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_GDP.pdf", width = 20, height = 15)
par(mfrow = c(4, 4))
fixest::iplot(sdid_total_releases_hgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_total_releases_lgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_hgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Air Emissions Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_lgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Air Emissions Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_hgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_lgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_hgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_lgdp, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_hgdp, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Surface Water Discharge Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_lgdp, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_hgdp, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Onsite Land Releases Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_lgdp, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Onsite Land Releases Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impound_hgdp, xlim = c(-3, 3), ylim = c(-0.02, 0.07), col = "blue",
              main = "Total Surface Impoundment Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impound_lgdp, xlim = c(-3, 3), ylim = c(-0.02, 0.07), col = "blue",
              main = "Total Surface Impoundment Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Is the MW Policy potentially Carcinogenic?
#======================================================================================================================#
### Onsite: Total releases intensity (carcinonogenic chemicals)
#======================================================================================================================#
sdid_total_releases_carcinogenic <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year):carcinogenic.chems +
    e.treated +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    carcinogenic.chems +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_carcinogenic, digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_carcinogenic, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_noncarcinogenic <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:carcinogenic.chems +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    carcinogenic.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_noncarcinogenic, digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (carcinogenic chemicals)
#======================================================================================================================#
sdid_air_carcinogenic <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year):carcinogenic.chems +
    e.treated +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_carcinogenic, digits = 3, digits.stats = 3)
fixest::etable(sdid_air_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_carcinogenic, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_noncarcinogenic <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:carcinogenic.chems +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_noncarcinogenic, digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (carcinogenic chemicals)
#======================================================================================================================#
sdid_point_air_carcinogenic <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year):carcinogenic.chems +
    e.treated +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_carcinogenic, digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_carcinogenic, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_noncarcinogenic <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:carcinogenic.chems +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_noncarcinogenic, digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (carcinogenic chemicals)
#======================================================================================================================#
sdid_fug_air_carcinogenic <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year):carcinogenic.chems +
    e.treated +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_carcinogenic, digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_carcinogenic, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_noncarcinogenic <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:carcinogenic.chems +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_noncarcinogenic, digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (carcinogenic chemicals)
#======================================================================================================================#
sdid_water_disc_carcinogenic <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year):carcinogenic.chems +
    e.treated +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_carcinogenic, digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_carcinogenic, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Surface Water Discharge Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_noncarcinogenic <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:carcinogenic.chems +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_noncarcinogenic, digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity (carcinogenic chemicals)
#======================================================================================================================#
sdid_land_releases_carcinogenic <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year):carcinogenic.chems +
    e.treated +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_carcinogenic, digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_carcinogenic, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
              main = "Total Land Releases Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_noncarcinogenic <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:carcinogenic.chems +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_noncarcinogenic, digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (carcinogenic chemicals)
#======================================================================================================================#
sdid_surface_impoundment_carcinogenic <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year):carcinogenic.chems +
    e.treated +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_carcinogenic, digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_carcinogenic, xlim = c(-3, 3), ylim = c(-0.05, 0.02), col = "blue",
              main = "Total Surface Impoundment Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_noncarcinogenic <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:carcinogenic.chems +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_noncarcinogenic, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.02, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_carcinogens.pdf", width = 20, height = 15)
par(mfrow = c(4, 4))
fixest::iplot(sdid_total_releases_carcinogenic, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_total_releases_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_carcinogenic, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_carcinogenic, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_carcinogenic, xlim = c(-3, 3), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_carcinogenic, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Surface Water Discharge Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_carcinogenic, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
              main = "Total Land Releases Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_carcinogenic, xlim = c(-3, 3), ylim = c(-0.05, 0.02), col = "blue",
              main = "Total Surface Impoundment Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.02, 0.085), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Does the MW Policy affect emissions intensity of the clean air act regulated chemicals?
#======================================================================================================================#
### Onsite: Total releases intensity (clean air act regulated chemicals)
#======================================================================================================================#
sdid_total_releases_caa <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year):clean.air.act.chems +
    e.treated +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
    # clean.air.act.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_caa, digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_caa, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_caa, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_caa, xlim = c(-3, 3), ylim = c(-0.8, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_noncaa <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:clean.air.act.chems +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
    # clean.air.act.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_noncaa, digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_noncaa, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (clean air act regulated chemicals) #TODO: Continue from here!
#======================================================================================================================#
sdid_air_caa <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year):clean.air.act.chems +
    e.treated +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
    # clean.air.act.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_caa, digits = 3, digits.stats = 3)
fixest::etable(sdid_air_caa, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_caa, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_caa, xlim = c(-3, 3), ylim = c(-0.5, 0.7), col = "blue",
              main = "Total Onsite Air Emissions Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_noncaa <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:clean.air.act.chems +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
    # clean.air.act.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_noncaa, digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_noncaa, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (clean air act regulated chemicals)
#======================================================================================================================#
sdid_point_air_caa <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year):clean.air.act.chems +
    e.treated +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
    # clean.air.act.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_caa, digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_caa, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_caa, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_caa, xlim = c(-3, 3), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total Point Air Emissions Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_noncaa <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:clean.air.act.chems +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
    # clean.air.act.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_noncaa, digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_noncaa, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (clean air act regulated chemicals)
#======================================================================================================================#
sdid_fug_air_caa <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year):clean.air.act.chems +
    e.treated +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
    # clean.air.act.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_caa, digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_caa, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_caa, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_caa, xlim = c(-3, 3), ylim = c(-0.5, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_noncaa <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:clean.air.act.chems +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
    # clean.air.act.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_noncaa, digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_noncaa, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (clean air act regulated chemicals)
#======================================================================================================================#
sdid_water_disc_caa <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year):clean.air.act.chems +
    e.treated +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
    # clean.air.act.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_caa, digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_caa, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_caa, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_caa, xlim = c(-3, 3), ylim = c(-0.8, 0.2), col = "blue",
              main = "Total Surface Water Discharge Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_noncaa <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:clean.air.act.chems +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
    # clean.air.act.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_noncaa, digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_noncaa, xlim = c(-3, 3), ylim = c(-0.3, 0.5), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity (clean air act regulated chemicals)
#======================================================================================================================#
sdid_land_releases_caa <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year):clean.air.act.chems +
    e.treated +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
    # clean.air.act.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_caa, digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_caa, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_caa, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_caa, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
              main = "Total Land Releases Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_noncaa <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:clean.air.act.chems +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
    # clean.air.act.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_noncaa, digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_noncaa, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Land Releases Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (clean air act regulated chemicals)
#======================================================================================================================#
sdid_surface_impoundment_caa <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year):clean.air.act.chems +
    e.treated +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
    # clean.air.act.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_caa, digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_caa, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_caa, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_caa, xlim = c(-3, 3), ylim = c(-0.05, 0.05), col = "blue",
              main = "Total Surface Impoundment Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_noncaa <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year) +
    e.treated:clean.air.act.chems +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
    # clean.air.act.chems +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_noncaa, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_noncaa, xlim = c(-3, 3), ylim = c(-0.02, 0.08), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_caa.pdf", width = 20, height = 15)
par(mfrow = c(4, 4))
fixest::iplot(sdid_total_releases_caa, xlim = c(-3, 3), ylim = c(-0.8, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_total_releases_noncaa, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_caa, xlim = c(-3, 3), ylim = c(-0.5, 0.7), col = "blue",
              main = "Total Onsite Air Emissions Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_noncaa, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_caa, xlim = c(-3, 3), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total Point Air Emissions Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_noncaa, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_caa, xlim = c(-3, 3), ylim = c(-0.5, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_noncaa, xlim = c(-3, 3), ylim = c(-0.3, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_caa, xlim = c(-3, 3), ylim = c(-0.8, 0.2), col = "blue",
              main = "Total Surface Water Discharge Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_noncaa, xlim = c(-3, 3), ylim = c(-0.3, 0.5), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_caa, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = "blue",
              main = "Total Land Releases Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_noncaa, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Land Releases Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_caa, xlim = c(-3, 3), ylim = c(-0.05, 0.05), col = "blue",
              main = "Total Surface Impoundment Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_noncaa, xlim = c(-3, 3), ylim = c(-0.02, 0.08), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Does the MW Policy affect emissions intensity of the Hazaradous Air Pollutants?
### TRI---General EPCRA Section 313 chemical
#======================================================================================================================#
### Onsite: Total releases intensity (Hazaradous Air Pollutants)
#======================================================================================================================#
sdid_total_releases_hap <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year):hap.chems +
      e.treated +
      treated:hap.chems +
      post:hap.chems +
      treated +
      hap.chems +
      post +
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
      # hap.chems +
      pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_hap, digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_hap, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_hap, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_hap, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_nonhap <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:hap.chems +
      treated:hap.chems +
      post:hap.chems +
      treated +
      hap.chems +
      post +
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
      # hap.chems +
      pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_nonhap, digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_nonhap, xlim = c(-3, 3), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total Onsite Releases Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (Hazaradous Air Pollutants)
#======================================================================================================================#
sdid_air_hap <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year):hap.chems +
      e.treated +
      treated:hap.chems +
      post:hap.chems +
      treated +
      hap.chems +
      post +
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
      # hap.chems +
      pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_hap, digits = 3, digits.stats = 3)
fixest::etable(sdid_air_hap, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_hap, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_hap, xlim = c(-3, 3), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total Onsite Air Emissions Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_nonhap <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:hap.chems +
      treated:hap.chems +
      post:hap.chems +
      treated +
      hap.chems +
      post +
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
      # hap.chems +
      pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_nonhap, digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_nonhap, xlim = c(-3, 3), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (Hazaradous Air Pollutants)
#======================================================================================================================#
sdid_point_air_hap <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year):hap.chems +
      e.treated +
      treated:hap.chems +
      post:hap.chems +
      treated +
      hap.chems +
      post +
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
      # hap.chems +
      pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_hap, digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_hap, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_hap, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_hap, xlim = c(-3, 3), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Point Air Emissions Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_nonhap <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:hap.chems +
      treated:hap.chems +
      post:hap.chems +
      treated +
      hap.chems +
      post +
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
      # hap.chems +
      pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_nonhap, digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_nonhap, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (Hazaradous Air Pollutants)
#======================================================================================================================#
sdid_fug_air_hap <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year):hap.chems +
      e.treated +
      treated:hap.chems +
      post:hap.chems +
      treated +
      hap.chems +
      post +
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
      # hap.chems +
      pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_hap, digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_hap, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_hap, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_hap, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_nonhap <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:hap.chems +
      treated:hap.chems +
      post:hap.chems +
      treated +
      hap.chems +
      post +
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
      # hap.chems +
      pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_nonhap, digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_nonhap, xlim = c(-3, 3), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (Hazaradous Air Pollutants)
#======================================================================================================================#
sdid_water_disc_hap <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year):hap.chems +
      e.treated +
      treated:hap.chems +
      post:hap.chems +
      treated +
      hap.chems +
      post +
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
      # hap.chems +
      pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_hap, digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_hap, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_hap, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_hap, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Surface Water Discharge Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_nonhap <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:hap.chems +
      treated:hap.chems +
      post:hap.chems +
      treated +
      hap.chems +
      post +
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
      # hap.chems +
      pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_nonhap, digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_nonhap, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity (Hazaradous Air Pollutants)
#======================================================================================================================#
sdid_land_releases_hap <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year):hap.chems +
      e.treated +
      treated:hap.chems +
      post:hap.chems +
      treated +
      hap.chems +
      post +
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
      # hap.chems +
      pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_hap, digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_hap, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_hap, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_hap, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Land Releases Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_nonhap <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:hap.chems +
      treated:hap.chems +
      post:hap.chems +
      treated +
      hap.chems +
      post +
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
      # hap.chems +
      pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_nonhap, digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_nonhap, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (Hazaradous Air Pollutants)
#======================================================================================================================#
sdid_surface_impoundment_hap <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year):hap.chems +
      e.treated +
      treated:hap.chems +
      post:hap.chems +
      treated +
      hap.chems +
      post +
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
      # hap.chems +
      pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_hap, digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_hap, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_hap, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_hap, xlim = c(-3, 3), ylim = c(-0.06, 0.06), col = "blue",
              main = "Total Surface Impoundment Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_nonhap <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:hap.chems +
      treated:hap.chems +
      post:hap.chems +
      treated +
      hap.chems +
      post +
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
      # hap.chems +
      pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_nonhap, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_nonhap, xlim = c(-3, 3), ylim = c(-0.02, 0.08), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_haps.pdf", width = 20, height = 14)
par(mfrow = c(4, 4))
fixest::iplot(sdid_total_releases_hap, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_total_releases_nonhap, xlim = c(-3, 3), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total Onsite Releases Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_hap, xlim = c(-3, 3), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total Onsite Air Emissions Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_nonhap, xlim = c(-3, 3), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_hap, xlim = c(-3, 3), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Point Air Emissions Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_nonhap, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_hap, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_nonhap, xlim = c(-3, 3), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_hap, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Surface Water Discharge Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_nonhap, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_hap, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Land Releases Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_nonhap, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_hap, xlim = c(-3, 3), ylim = c(-0.06, 0.06), col = "blue",
              main = "Total Surface Impoundment Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_nonhap, xlim = c(-3, 3), ylim = c(-0.02, 0.08), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Does the MW Policy affect emissions intensity of the Persistent Bioaccumulative Toxic Chemicals?
#======================================================================================================================#
### Onsite: Total releases intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
sdid_total_releases_pbt <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year):pbt.chems +
      e.treated +
      treated:pbt.chems +
      post:pbt.chems +
      treated +
      pbt.chems +
      post +
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
      hap.chems
      # pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_pbt, digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_pbt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_pbt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_pbt, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_nonpbt <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:pbt.chems +
      treated:pbt.chems +
      post:pbt.chems +
      treated +
      pbt.chems +
      post +
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
      hap.chems
      # pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_nonpbt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_nonpbt, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
sdid_air_pbt <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year):pbt.chems +
      e.treated +
      treated:pbt.chems +
      post:pbt.chems +
      treated +
      pbt.chems +
      post +
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
      hap.chems
      # pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_pbt, digits = 3, digits.stats = 3)
fixest::etable(sdid_air_pbt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_pbt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_pbt, xlim = c(-3, 3), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total Air Emissions Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_nonpbt <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:pbt.chems +
      treated:pbt.chems +
      post:pbt.chems +
      treated +
      pbt.chems +
      post +
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
      hap.chems
      # pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_nonpbt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_nonpbt, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Air Emissions Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
sdid_point_air_pbt <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year):pbt.chems +
      e.treated +
      treated:pbt.chems +
      post:pbt.chems +
      treated +
      pbt.chems +
      post +
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
      hap.chems
      # pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_pbt, digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_pbt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_pbt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_pbt, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_nonpbt <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:pbt.chems +
      treated:pbt.chems +
      post:pbt.chems +
      treated +
      pbt.chems +
      post +
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
      hap.chems
      # pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_nonpbt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_nonpbt, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
sdid_fug_air_pbt <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year):pbt.chems +
      e.treated +
      treated:pbt.chems +
      post:pbt.chems +
      treated +
      pbt.chems +
      post +
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
      hap.chems
      # pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_pbt, digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_pbt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_pbt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_pbt, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_nonpbt <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:pbt.chems +
      treated:pbt.chems +
      post:pbt.chems +
      treated +
      pbt.chems +
      post +
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
      hap.chems
      # pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_nonpbt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_nonpbt, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
sdid_water_disc_pbt <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year):pbt.chems +
      e.treated +
      treated:pbt.chems +
      post:pbt.chems +
      treated +
      pbt.chems +
      post +
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
      hap.chems
      # pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_pbt, digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_pbt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_pbt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_pbt, xlim = c(-3, 3), ylim = c(-0.3, 0.8), col = "blue",
              main = "Total Surface Water Discharge Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_nonpbt <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:pbt.chems +
      treated:pbt.chems +
      post:pbt.chems +
      treated +
      pbt.chems +
      post +
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
      hap.chems
      # pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_nonpbt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_nonpbt, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
sdid_land_releases_pbt <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year):pbt.chems +
      e.treated +
      treated:pbt.chems +
      post:pbt.chems +
      treated +
      pbt.chems +
      post +
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
      hap.chems
      # pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_pbt, digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_pbt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_pbt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_pbt, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
              main = "Total Land Releases Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_nonpbt <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:pbt.chems +
      treated:pbt.chems +
      post:pbt.chems +
      treated +
      pbt.chems +
      post +
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
      hap.chems
      # pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_nonpbt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_nonpbt, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Land Releases Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
sdid_surface_impoundment_pbt <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year):pbt.chems +
      e.treated +
      treated:pbt.chems +
      post:pbt.chems +
      treated +
      pbt.chems +
      post +
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
      hap.chems
      # pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_pbt, digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_pbt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_pbt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_pbt, xlim = c(-3, 3), ylim = c(-0.05, 0.05), col = "blue",
              main = "Total Surface Impoundment Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_nonpbt <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:pbt.chems +
      treated:pbt.chems +
      post:pbt.chems +
      treated +
      pbt.chems +
      post +
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
      hap.chems
      # pbt.chems
      |
      year +
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_nonpbt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_nonpbt, xlim = c(-3, 3), ylim = c(-0.02, 0.08), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_pbts.pdf", width = 20, height = 15)
par(mfrow = c(4, 4))
fixest::iplot(sdid_total_releases_pbt, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_total_releases_nonpbt, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_pbt, xlim = c(-3, 3), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total Air Emissions Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_nonpbt, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Air Emissions Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_pbt, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_nonpbt, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_pbt, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_nonpbt, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_pbt, xlim = c(-3, 3), ylim = c(-0.3, 0.8), col = "blue",
              main = "Total Surface Water Discharge Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_nonpbt, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_pbt, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
              main = "Total Land Releases Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_nonpbt, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Land Releases Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_pbt, xlim = c(-3, 3), ylim = c(-0.05, 0.05), col = "blue",
              main = "Total Surface Impoundment Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_nonpbt, xlim = c(-3, 3), ylim = c(-0.02, 0.08), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Labour intensive and capital intensive industries
### Are labour-intensive industries more responsive to MW hikes?
### Are capital-intensive industries more resilient to MW hikes?
#======================================================================================================================#
triQc <- triQc %>% mutate(
  capital.intensity = cap / vadd, #ratio of total output to total capital expenditure
  capital.intensity.high = as.numeric(capital.intensity > median(capital.intensity)),
  labour.intensity = prodh / vadd, #ratio of total output to total production wages
  # labour.intensity = prodw / vship, #ratio of total production wages to total revenue
  labour.intensity.high = as.numeric(labour.intensity > median(labour.intensity)),
)
table(triQc$capital.intensity.high)
table(triQc$labour.intensity.high)
#======================================================================================================================#
### Onsite: Total releases intensity (labour intensive industries)
#======================================================================================================================#
sdid_total_releases_highlabint <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year):labour.intensity.high +
      e.treated +
      treated:labour.intensity.high +
      post:labour.intensity.high +
      treated +
      labour.intensity.high +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_highlabint, digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_highlabint, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_highlabint, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_highlabint, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_lowlabint <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:labour.intensity.high +
      treated:labour.intensity.high +
      post:labour.intensity.high +
      treated +
      labour.intensity.high +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_lowlabint, digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_lowlabint, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (labour intensive industries)
#======================================================================================================================#
sdid_air_highlabint <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year):labour.intensity.high +
      e.treated +
      treated:labour.intensity.high +
      post:labour.intensity.high +
      treated +
      labour.intensity.high +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_highlabint, digits = 3, digits.stats = 3)
fixest::etable(sdid_air_highlabint, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_highlabint, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_highlabint, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Air Emissions Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_lowlabint <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:labour.intensity.high +
      treated:labour.intensity.high +
      post:labour.intensity.high +
      treated +
      labour.intensity.high +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_lowlabint, digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_lowlabint, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Air Emissions Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (labour intensive industries)
#======================================================================================================================#
sdid_point_air_highlabint <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year):labour.intensity.high +
      e.treated +
      treated:labour.intensity.high +
      post:labour.intensity.high +
      treated +
      labour.intensity.high +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_highlabint, digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_highlabint, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_highlabint, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_highlabint, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Point Air Emissions Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_lowlabint <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:labour.intensity.high +
      treated:labour.intensity.high +
      post:labour.intensity.high +
      treated +
      labour.intensity.high +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_lowlabint, digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_lowlabint, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Point Air Emissions Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (labour intensive industries)
#======================================================================================================================#
sdid_fug_air_highlabint <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year):labour.intensity.high +
      e.treated +
      treated:labour.intensity.high +
      post:labour.intensity.high +
      treated +
      labour.intensity.high +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_highlabint, digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_highlabint, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_highlabint, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_highlabint, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_lowlabint <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:labour.intensity.high +
      treated:labour.intensity.high +
      post:labour.intensity.high +
      treated +
      labour.intensity.high +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_lowlabint, digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_lowlabint, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (labour intensive industries)
#======================================================================================================================#
sdid_water_disc_highlabint <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year):labour.intensity.high +
      e.treated +
      treated:labour.intensity.high +
      post:labour.intensity.high +
      treated +
      labour.intensity.high +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_highlabint, digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_highlabint, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_highlabint, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_highlabint, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Surface Water Discharge Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_lowlabint <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:labour.intensity.high +
      treated:labour.intensity.high +
      post:labour.intensity.high +
      treated +
      labour.intensity.high +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_lowlabint, digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_lowlabint, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity (labour intensive industries)
#======================================================================================================================#
sdid_land_releases_highlabint <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year):labour.intensity.high +
      e.treated +
      treated:labour.intensity.high +
      post:labour.intensity.high +
      treated +
      labour.intensity.high +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_highlabint, digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_highlabint, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_highlabint, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_highlabint, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_lowlabint <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:labour.intensity.high +
      treated:labour.intensity.high +
      post:labour.intensity.high +
      treated +
      labour.intensity.high +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_lowlabint, digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_lowlabint, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (labour intensive industries)
#======================================================================================================================#
sdid_surface_impoundment_highlabint <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year):labour.intensity.high +
      e.treated +
      treated:labour.intensity.high +
      post:labour.intensity.high +
      treated +
      labour.intensity.high +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_highlabint, digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_highlabint, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_highlabint, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_highlabint, xlim = c(-3, 3), ylim = c(-0.05, 0.04), col = "blue",
              main = "Total Surface Impoundment Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_lowlabint <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:labour.intensity.high +
      treated:labour.intensity.high +
      post:labour.intensity.high +
      treated +
      labour.intensity.high +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_lowlabint, digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_lowlabint, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_lowlabint, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_lowlabint, xlim = c(-3, 3), ylim = c(-0.02, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_highlabint.pdf", width = 20, height = 15)
par(mfrow = c(4, 4))
fixest::iplot(sdid_total_releases_highlabint, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_total_releases_lowlabint, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_highlabint, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Air Emissions Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_lowlabint, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Air Emissions Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_highlabint, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Point Air Emissions Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_lowlabint, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Point Air Emissions Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_highlabint, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_lowlabint, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_highlabint, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Surface Water Discharge Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_lowlabint, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_highlabint, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_lowlabint, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_highlabint, xlim = c(-3, 3), ylim = c(-0.05, 0.04), col = "blue",
              main = "Total Surface Impoundment Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_lowlabint, xlim = c(-3, 3), ylim = c(-0.02, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Heifindahl Hirschman Index (HHI)---Industry concentration
#======================================================================================================================#
triQc <- triQc %>%
  group_by(naics.code) %>%
  mutate(
    revenue.2 = vship^2, # get the squared revenue by industry
    sum.revenue.2 = sum(revenue.2, na.rm = T), # get the sum of squared revenue by industry
  ) %>%
  group_by(year, naics.code) %>%
  mutate(
    hhi = mean(sum.revenue.2, na.rm = T), # get the mean of squared revenue by industry to get HHI
  )

# low HHI, less industry concentration and more competition
triQc$low.ind.conc <- as.numeric(triQc$hhi < median(triQc$hhi))
sum_up(triQc, c(vship, revenue.2, sum.revenue.2, hhi, low.ind.conc))
table(triQc$low.ind.conc)
#======================================================================================================================#
### Onsite: Total releases intensity (industry concentration)
#======================================================================================================================#
sdid_total_releases_lowindconc <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year):low.ind.conc +
      e.treated +
      treated:low.ind.conc +
      post:low.ind.conc +
      treated +
      low.ind.conc +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_lowindconc, digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_lowindconc, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_lowindconc, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_lowindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_highindconc <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:low.ind.conc +
      treated:low.ind.conc +
      post:low.ind.conc +
      treated +
      low.ind.conc +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_highindconc, digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_highindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (industry concentration)
#======================================================================================================================#
sdid_air_lowindconc <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year):low.ind.conc +
      e.treated +
      treated:low.ind.conc +
      post:low.ind.conc +
      treated +
      low.ind.conc +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_lowindconc, digits = 3, digits.stats = 3)
fixest::etable(sdid_air_lowindconc, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_lowindconc, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_lowindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Air Emissions Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_highindconc <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:low.ind.conc +
      treated:low.ind.conc +
      post:low.ind.conc +
      treated +
      low.ind.conc +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_highindconc, digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_highindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Air Emissions Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (industry concentration)
#======================================================================================================================#
sdid_point_air_lowindconc <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year):low.ind.conc +
      e.treated +
      treated:low.ind.conc +
      post:low.ind.conc +
      treated +
      low.ind.conc +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_lowindconc, digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_lowindconc, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_lowindconc, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_lowindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_highindconc <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:low.ind.conc +
      treated:low.ind.conc +
      post:low.ind.conc +
      treated +
      low.ind.conc +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_highindconc, digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_highindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (industry concentration)
#======================================================================================================================#
sdid_fug_air_lowindconc <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year):low.ind.conc +
      e.treated +
      treated:low.ind.conc +
      post:low.ind.conc +
      treated +
      low.ind.conc +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_lowindconc, digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_lowindconc, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_lowindconc, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_lowindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_highindconc <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:low.ind.conc +
      treated:low.ind.conc +
      post:low.ind.conc +
      treated +
      low.ind.conc +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_highindconc, digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_highindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (industry concentration)
#======================================================================================================================#
sdid_water_disc_lowindconc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year):low.ind.conc +
      e.treated +
      treated:low.ind.conc +
      post:low.ind.conc +
      treated +
      low.ind.conc +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_lowindconc, digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_lowindconc, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_lowindconc, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_lowindconc, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = "blue",
              main = "Total Surface Water Discharge Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_highindconc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:low.ind.conc +
      treated:low.ind.conc +
      post:low.ind.conc +
      treated +
      low.ind.conc +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_highindconc, digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_highindconc, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity (industry concentration)
#======================================================================================================================#
sdid_land_releases_lowindconc <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year):low.ind.conc +
      e.treated +
      treated:low.ind.conc +
      post:low.ind.conc +
      treated +
      low.ind.conc +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_lowindconc, digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_lowindconc, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_lowindconc, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_lowindconc, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Land Releases Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_highindconc <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:low.ind.conc +
      treated:low.ind.conc +
      post:low.ind.conc +
      treated +
      low.ind.conc +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_highindconc, digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_highindconc, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Land Releases Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (industry concentration)
#======================================================================================================================#
sdid_surface_impoundment_lowindconc <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year):low.ind.conc +
      e.treated +
      treated:low.ind.conc +
      post:low.ind.conc +
      treated +
      low.ind.conc +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_lowindconc, digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_lowindconc, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_lowindconc, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_lowindconc, xlim = c(-3, 3), ylim = c(-0.06, 0.04), col = "blue",
              main = "Total Surface Impoundment Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_highindconc <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year) +
      e.treated:low.ind.conc +
      treated:low.ind.conc +
      post:low.ind.conc +
      treated +
      low.ind.conc +
      post +
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
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_highindconc, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_highindconc, xlim = c(-3, 3), ylim = c(-0.02, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_lowindconc.pdf", width = 20, height = 15)
par(mfrow = c(4, 4))
fixest::iplot(sdid_total_releases_lowindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_total_releases_highindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_lowindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Air Emissions Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_highindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Air Emissions Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_lowindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_highindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_lowindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_highindconc, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_lowindconc, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = "blue",
              main = "Total Surface Water Discharge Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_highindconc, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_lowindconc, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Land Releases Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_highindconc, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Land Releases Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_lowindconc, xlim = c(-3, 3), ylim = c(-0.06, 0.04), col = "blue",
              main = "Total Surface Impoundment Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_highindconc, xlim = c(-3, 3), ylim = c(-0.02, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#