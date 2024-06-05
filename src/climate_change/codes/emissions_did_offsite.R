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
  l.total.releases.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
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
  l.total.releases.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
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
  l.total.releases.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
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
fixest::iplot(did_releases, xlim = c(2011, 2017), ylim = c(-0.8, 0.8), col = "blue",
              main = "Total Offsite Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_releases)[grep(pattern = "rel.year", names(coef(did_releases)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_releases, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Offsite: Total releases, unknown
#======================================================================================================================#
did_releases_unknown <- fixest::feols(
  l.total.releases.unknown.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_releases_unknown, digits = 4, digits.stats = 4)

did_releases_unknown <- fixest::feols(
  l.total.releases.unknown.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
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
fixest::etable(did_releases_unknown, digits = 4, digits.stats = 4)

did_releases_unknown <- fixest::feols(
  l.total.releases.unknown.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
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
fixest::etable(did_releases_unknown, digits = 4, digits.stats = 4)
fixest::iplot(did_releases_unknown, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Offsite Releases Intensity, Unknown", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_releases_unknown)[grep(pattern = "rel.year", names(coef(did_releases_unknown)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_releases_unknown, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Offsite: Total releases, waste broker
#======================================================================================================================#
did_releases_wastebroker <- fixest::feols(
  l.total.releases.wastebroker.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_releases_wastebroker, digits = 4, digits.stats = 4)

did_releases_wastebroker <- fixest::feols(
  l.total.releases.wastebroker.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
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
fixest::etable(did_releases_wastebroker, digits = 4, digits.stats = 4)

did_releases_wastebroker <- fixest::feols(
  l.total.releases.wastebroker.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, offsite.state.id),
)
fixest::etable(did_releases_wastebroker, digits = 4, digits.stats = 4)
fixest::iplot(did_releases_wastebroker, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Offsite Releases Intensity, Waste Broker", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_releases_wastebroker)[grep(pattern = "rel.year", names(coef(did_releases_wastebroker)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_releases_wastebroker, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Offsite: Total releases, others
#======================================================================================================================#
did_releases_others <- fixest::feols(
  l.total.releases.other.mgt.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_releases_others, digits = 4, digits.stats = 4)

did_releases_others <- fixest::feols(
  l.total.releases.other.mgt.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
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
fixest::etable(did_releases_others, digits = 4, digits.stats = 4)

did_releases_others <- fixest::feols(
  l.total.releases.other.mgt.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, offsite.state.id),
)
fixest::etable(did_releases_others, digits = 4, digits.stats = 4)
fixest::iplot(did_releases_others, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Offsite Releases Intensity, Others", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_releases_others)[grep(pattern = "rel.year", names(coef(did_releases_others)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_releases_others, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Offsite: Total releases, metal solidification
#======================================================================================================================#
did_releases_metalsolidify <- fixest::feols(
  l.total.releases.metalsolidify.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_releases_metalsolidify, digits = 4, digits.stats = 4)

did_releases_metalsolidify <- fixest::feols(
  l.total.releases.metalsolidify.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
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
fixest::etable(did_releases_metalsolidify, digits = 4, digits.stats = 4)

did_releases_metalsolidify <- fixest::feols(
  l.total.releases.metalsolidify.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, offsite.state.id),
)
fixest::etable(did_releases_metalsolidify, digits = 4, digits.stats = 4)
fixest::iplot(did_releases_metalsolidify, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Offsite Releases Intensity, Metal Solidification", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_releases_metalsolidify)[grep(pattern = "rel.year", names(coef(did_releases_metalsolidify)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_releases_metalsolidify, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Offsite: Total releases, storage
#======================================================================================================================#
did_releases_storage <- fixest::feols(
  l.total.releases.storage.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_releases_storage, digits = 4, digits.stats = 4)

did_releases_storage <- fixest::feols(
  l.total.releases.storage.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
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
fixest::etable(did_releases_storage, digits = 4, digits.stats = 4)

did_releases_storage <- fixest::feols(
  l.total.releases.storage.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, offsite.state.id),
)
fixest::etable(did_releases_storage, digits = 4, digits.stats = 4)
fixest::iplot(did_releases_storage, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Offsite Releases Intensity, Storage", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_releases_storage)[grep(pattern = "rel.year", names(coef(did_releases_storage)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_releases_storage, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "../latex/fig_sdid_total_releases_offsite.pdf", width = 12, height = 7)
par(mfrow = c(2, 3))
fixest::iplot(did_releases, xlim = c(2011, 2017), ylim = c(-0.8, 0.8), col = "blue",
              main = "Total Offsite Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_releases_unknown, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Offsite Releases Intensity, Unknown", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_releases_wastebroker, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Offsite Releases Intensity, Waste Broker", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_releases_others, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Offsite Releases Intensity, Others", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_releases_metalsolidify, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Offsite Releases Intensity, Metal Solidification", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_releases_storage, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Offsite Releases Intensity, Storage", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Offsite: Total land releases
#======================================================================================================================#
did_land_releases <- fixest::feols(
  l.total.land.releases.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_land_releases, digits = 4, digits.stats = 4)

did_land_releases <- fixest::feols(
  l.total.land.releases.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
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
fixest::etable(did_land_releases, digits = 4, digits.stats = 4)

did_land_releases <- fixest::feols(
  l.total.land.releases.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, offsite.state.id),
)
fixest::etable(did_land_releases, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Offsite Land Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_land_releases)[grep(pattern = "rel.year", names(coef(did_land_releases)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_land_releases, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Offsite: Total land releases, others
#======================================================================================================================#
did_land_releases_others <- fixest::feols(
  l.total.land.releases.other.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_land_releases_others, digits = 4, digits.stats = 4)

did_land_releases_others <- fixest::feols(
  l.total.land.releases.other.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
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
fixest::etable(did_land_releases_others, digits = 4, digits.stats = 4)

did_land_releases_others <- fixest::feols(
  l.total.land.releases.other.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, offsite.state.id),
)
fixest::etable(did_land_releases_others, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_others, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Offsite Land Releases Intensity, Others", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_land_releases_others)[grep(pattern = "rel.year", names(coef(did_land_releases_others)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_land_releases_others, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Offsite: Total waste water releases
#======================================================================================================================#
did_waste_water_releases <- fixest::feols(
  l.total.wastewater.releases.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
    )
    |
    csw(,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_waste_water_releases, digits = 4, digits.stats = 4)

did_waste_water_releases <- fixest::feols(
  l.total.wastewater.releases.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
    )
    |
    csw(,
      year,
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
fixest::etable(did_waste_water_releases, digits = 4, digits.stats = 4)

did_waste_water_releases <- fixest::feols(
  l.total.wastewater.releases.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
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
fixest::etable(did_waste_water_releases, digits = 4, digits.stats = 4)
fixest::iplot(did_waste_water_releases, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Offsite Waste Water Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_waste_water_releases)[grep(pattern = "rel.year", names(coef(did_waste_water_releases)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_waste_water_releases, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Offsite: Total surface impoundment
#======================================================================================================================#
did_surface_impoundment <- fixest::feols(
  l.total.surface.impoundment.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_surface_impoundment, digits = 4, digits.stats = 4)

did_surface_impoundment <- fixest::feols(
  l.total.surface.impoundment.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
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
fixest::etable(did_surface_impoundment, digits = 4, digits.stats = 4)

did_surface_impoundment <- fixest::feols(
  l.total.surface.impoundment.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, offsite.state.id),
)
fixest::etable(did_surface_impoundment, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impoundment, xlim = c(2011, 2017), ylim = c(-0.05, 0.05), col = "blue",
              main = "Total Surface Impoundment Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_surface_impoundment)[grep(pattern = "rel.year", names(coef(did_surface_impoundment)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_surface_impoundment, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Offsite: Total toland treatment
#======================================================================================================================#
did_toland_treatment <- fixest::feols(
  l.total.releases.toland.treatment.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_toland_treatment, digits = 4, digits.stats = 4)

did_toland_treatment <- fixest::feols(
  l.total.releases.toland.treatment.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
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
fixest::etable(did_toland_treatment, digits = 4, digits.stats = 4)

did_toland_treatment <- fixest::feols(
  l.total.releases.toland.treatment.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, offsite.state.id),
)
fixest::etable(did_toland_treatment, digits = 4, digits.stats = 4)
fixest::iplot(did_toland_treatment, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total toland treatment Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_toland_treatment)[grep(pattern = "rel.year", names(coef(did_toland_treatment)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_toland_treatment, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Offsite: Total landfills
#======================================================================================================================#
did_landfills <- fixest::feols(
  l.total.landfills.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_landfills, digits = 4, digits.stats = 4)

did_landfills <- fixest::feols(
  l.total.landfills.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
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
fixest::etable(did_landfills, digits = 4, digits.stats = 4)

did_landfills <- fixest::feols(
  l.total.landfills.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, offsite.state.id),
)
fixest::etable(did_landfills, digits = 4, digits.stats = 4)
fixest::iplot(did_landfills, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Landfills Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_landfills)[grep(pattern = "rel.year", names(coef(did_landfills)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_landfills, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Offsite: Total Underground injection
#======================================================================================================================#
did_underground_injection <- fixest::feols(
  l.total.underground.injection.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      facility.id,
      treated.cluster.id,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_underground_injection, digits = 4, digits.stats = 4)

did_underground_injection <- fixest::feols(
  l.total.underground.injection.offsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        production.ratio.activity.index +
        maxnum.chem.onsite
    )
    |
    csw(,
      year,
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
fixest::etable(did_underground_injection, digits = 4, digits.stats = 4)

did_underground_injection <- fixest::feols(
  l.total.underground.injection.offsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      federal.facility +
      produced.chem.facility +
      imported.chem.facility +
      production.ratio.activity.index +
      maxnum.chem.onsite
      |
      year +
        facility.id +
        fips.code +
        facility.county +
        treated.cluster.id +
        facility.state.id +
        chemical.id +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, offsite.state.id),
)
fixest::etable(did_underground_injection, digits = 4, digits.stats = 4)
fixest::iplot(did_underground_injection, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Underground Injection Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(did_underground_injection)[grep(pattern = "rel.year", names(coef(did_underground_injection)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(did_underground_injection, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "../latex/fig_sdid_total_land_releases_offsite.pdf", width = 12, height = 10)
par(mfrow = c(3, 3))
fixest::iplot(did_waste_water_releases, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Offsite Waste Water Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases, xlim = c(2011, 2017), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total Offsite Land Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_others, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Offsite Land Releases Intensity, Others", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment, xlim = c(2011, 2017), ylim = c(-0.05, 0.05), col = "blue",
              main = "Total Surface Impoundment Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_toland_treatment, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total toland treatment Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_landfills, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Landfills Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_underground_injection, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Underground Injection Intensity, Offsite", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#