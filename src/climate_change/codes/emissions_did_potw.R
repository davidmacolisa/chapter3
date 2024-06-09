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
file <- "./Data_PhD/US/BLS/offsite/triQc_potw.rds"
triQc <- read_rds(file = file)
#======================================================================================================================#
### Labour Cost---Wage per hr, weekly wages, and  total wages
#======================================================================================================================#
table(triQc$facility.state, triQc$ch.year)
n_distinct(triQc$chemical.name)
sort(unique(triQc$chemical.name))
sort(unique(triQc$potw.zipcode))
sort(unique(triQc$facility.state.id))
sort(unique(triQc$year))
sort(unique(triQc$rel.year))
#======================================================================================================================#
### POTW: Total releases
#======================================================================================================================#
sum_up(triQc, c(total.potw.releases.offsite, potw.releases.underground.other.offsite,
                potw.releases.underground.Iwells.offsite,
                potw.treatment.offsite, total.potw.management.offsite))

did_releases <- fixest::feols(
  l.total.potw.releases.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    # federal.facility +
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
fixest::etable(did_releases, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_releases, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_releases, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total POTW Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### POTW: Total underground releases
#======================================================================================================================#
did_underground_releases <- fixest::feols(
  l.potw.releases.underground.Iwells.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    # federal.facility +
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
fixest::etable(did_underground_releases, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_underground_releases, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_underground_releases, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total POTW Underground Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### POTW: Total underground releases, other
#======================================================================================================================#
did_underground_releases_other <- fixest::feols(
  l.potw.releases.underground.other.offsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    # federal.facility +
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
fixest::etable(did_underground_releases_other, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_underground_releases_other, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_underground_releases_other, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total POTW Underground Releases Intensity, other", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### POTW: Treatment Offsite
#======================================================================================================================#
did_treatment <- fixest::feols(
  l.potw.treatment.offsite ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    # federal.facility +
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
fixest::etable(did_treatment, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_treatment, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_treatment, xlim = c(-3, 3), ylim = c(-0.8, 0.8), col = "blue",
              main = "Total POTW Treatment Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### POTW: Total POTW Waste Management
#======================================================================================================================#
did_potw_waste_mgt <- fixest::feols(
  l.total.potw.management.offsite ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    # federal.facility +
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
fixest::etable(did_potw_waste_mgt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(did_potw_waste_mgt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(did_potw_waste_mgt, xlim = c(-3, 3), ylim = c(-0.8, 0.8), col = "blue",
              main = "Total POTW Waste Management", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_releases_potws.pdf", width = 12, height = 7)
par(mfrow = c(2, 3))
fixest::iplot(did_releases, xlim = c(-3, 3), ylim = c(-0.4, 0.5), col = "blue",
              main = "Total POTW Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_underground_releases, xlim = c(-3, 3), ylim = c(-0.5, 0.3), col = "blue",
              main = "Total POTW Underground Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_underground_releases_other, xlim = c(-3, 3), ylim = c(-0.4, 0.5), col = "blue",
              main = "Total POTW Underground Releases Intensity, Other", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_treatment, xlim = c(-3, 3), ylim = c(-0.8, 0.8), col = "blue",
              main = "Total Offsite Treatment, POTW", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_potw_waste_mgt, xlim = c(-3, 3), ylim = c(-0.8, 0.8), col = "blue",
              main = "Total Offsite Waste Management, POTW", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#