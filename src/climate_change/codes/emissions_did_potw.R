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
  l.total.potw.releases.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        # federal.facility +
        entire.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      potw.id,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_releases, digits = 4, digits.stats = 4)

did_releases <- fixest::feols(
  l.total.potw.releases.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        # federal.facility +
        entire.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
      potw.id,
      facility.id,
      fips.code,
      facility.county,
      treated.cluster.id,
      facility.state.id,
      chemical.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_releases, digits = 4, digits.stats = 4)

did_releases <- fixest::feols(
  l.total.potw.releases.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      # federal.facility +
      entire.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        potw.id +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_releases, digits = 4, digits.stats = 4)
fixest::iplot(did_releases, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total POTW Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_releases)[grep(pattern = "rel.year", names(coef(did_releases)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_releases, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### POTW: Total underground releases
#======================================================================================================================#
did_underground_releases <- fixest::feols(
  l.potw.releases.underground.Iwells.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        # federal.facility +
        entire.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      potw.id,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_underground_releases, digits = 4, digits.stats = 4)

did_underground_releases <- fixest::feols(
  l.potw.releases.underground.Iwells.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        # federal.facility +
        entire.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
      potw.id,
      facility.id,
      fips.code,
      facility.county,
      treated.cluster.id,
      facility.state.id,
      chemical.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_underground_releases, digits = 4, digits.stats = 4)

did_underground_releases <- fixest::feols(
  l.potw.releases.underground.Iwells.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      # federal.facility +
      entire.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        potw.id +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_underground_releases, digits = 4, digits.stats = 4)
fixest::iplot(did_underground_releases, xlim = c(2011, 2017), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total POTW Underground Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_underground_releases)[grep(pattern = "rel.year", names(coef(did_underground_releases)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_underground_releases, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### POTW: Total underground releases, other
#======================================================================================================================#
did_underground_releases_other <- fixest::feols(
  l.potw.releases.underground.other.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        # federal.facility +
        entire.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      potw.id,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_underground_releases_other, digits = 4, digits.stats = 4)

did_underground_releases_other <- fixest::feols(
  l.potw.releases.underground.other.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        # federal.facility +
        entire.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
      potw.id,
      facility.id,
      fips.code,
      facility.county,
      treated.cluster.id,
      facility.state.id,
      chemical.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_underground_releases_other, digits = 4, digits.stats = 4)

did_underground_releases_other <- fixest::feols(
  l.potw.releases.underground.other.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      # federal.facility +
      entire.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        potw.id +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_underground_releases_other, digits = 4, digits.stats = 4)
fixest::iplot(did_underground_releases_other, xlim = c(2011, 2017), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total POTW Underground Releases Intensity, Other", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_underground_releases_other)[grep(pattern = "rel.year", names(coef(did_underground_releases_other)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_underground_releases_other, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### POTW: Treatment Offsite
#======================================================================================================================#
did_treatment <- fixest::feols(
  l.potw.treatment.offsite ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        # federal.facility +
        entire.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      potw.id,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_treatment, digits = 4, digits.stats = 4)

did_treatment <- fixest::feols(
  l.potw.treatment.offsite ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        # federal.facility +
        entire.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
      potw.id,
      facility.id,
      fips.code,
      facility.county,
      treated.cluster.id,
      facility.state.id,
      chemical.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_treatment, digits = 4, digits.stats = 4)

did_treatment <- fixest::feols(
  l.potw.treatment.offsite ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      # federal.facility +
      entire.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        potw.id +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_treatment, digits = 4, digits.stats = 4)
fixest::iplot(did_treatment, xlim = c(2011, 2017), ylim = c(-0.8, 0.8), col = "blue",
              main = "Total Offsite Treatment, POTW", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_treatment)[grep(pattern = "rel.year", names(coef(did_treatment)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_treatment, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### POTW: Total POTW Waste Management
#======================================================================================================================#
did_potw_waste_mgt <- fixest::feols(
  l.total.potw.management.offsite ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        # federal.facility +
        entire.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      potw.id,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_potw_waste_mgt, digits = 4, digits.stats = 4)

did_potw_waste_mgt <- fixest::feols(
  l.total.potw.management.offsite ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        # federal.facility +
        entire.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
      potw.id,
      facility.id,
      fips.code,
      facility.county,
      treated.cluster.id,
      facility.state.id,
      chemical.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_potw_waste_mgt, digits = 4, digits.stats = 4)

did_potw_waste_mgt <- fixest::feols(
  l.total.potw.management.offsite ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      # federal.facility +
      entire.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        potw.id +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_potw_waste_mgt, digits = 4, digits.stats = 4)
fixest::iplot(did_potw_waste_mgt, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Offsite Waste Management, POTW", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_potw_waste_mgt)[grep(pattern = "rel.year", names(coef(did_potw_waste_mgt)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_potw_waste_mgt, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "../latex/fig_sdid_total_releases_potws.pdf", width = 12, height = 7)
par(mfrow = c(2, 3))
fixest::iplot(did_releases, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total POTW Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_underground_releases, xlim = c(2011, 2017), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total POTW Underground Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_underground_releases_other, xlim = c(2011, 2017), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total POTW Underground Releases Intensity, Other", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_treatment, xlim = c(2011, 2017), ylim = c(-1.5, 1.5), col = "blue",
              main = "Total Offsite Treatment, POTW", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_potw_waste_mgt, xlim = c(2011, 2017), ylim = c(-1.5, 1.5), col = "blue",
              main = "Total Offsite Waste Management, POTW", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#