#======================================================================================================================#
### PhD Chapter 3
### Indirect Consequences of a Raising Minimum Wage
### 30 October 2023
### Use Regression Discontinuity Analysis
#======================================================================================================================#
### Packages
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
file <- "./Data_PhD/US/BLS/onsite/triQc_on.rds"
triQc <- read_rds(file = file)

sort(unique(triQc$year))
sort(unique(triQc$rel.year))
#======================================================================================================================#
### Onsite: Total releases intensity
#======================================================================================================================#
did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ e.treated +
    sw0(
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
        maxnum.chem.onsite
    )
    |
    csw(,
      facility.id,
      treated.cluster.id,
      chemical.year.fe,
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

start_time <- Sys.time()
did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ e.treated +
    sw0(
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
      chemical.year.fe,
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
end_time <- Sys.time()
end_time - start_time
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
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
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases)[grep(pattern = "rel.year", names(coef(did_total_releases)))]
pre.treat.coef <- pre.treat.coef[4:5]
linearHypothesis(did_total_releases, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_did_total_releases_onsite_int.pdf", width = 5, height = 4.5)
# par(mfrow = c(1, 2))
fixest::iplot(did_total_releases, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Onsite: Total air emissions intensity
#======================================================================================================================#
did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        private.facility +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        chemical.formulation.component +
        chemical.article.component +
        chemical.manufacturing.aid +
        chemical.ancilliary.use +
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
fixest::etable(did_air, digits = 4, digits.stats = 4)

did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        cpi.1 +
        private.facility +
        federal.facility +
        produced.chem.facility +
        imported.chem.facility +
        chemical.formulation.component +
        chemical.article.component +
        chemical.manufacturing.aid +
        chemical.ancilliary.use +
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
fixest::etable(did_air, digits = 4, digits.stats = 4)

did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
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
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_air, digits = 4, digits.stats = 4)
fixest::iplot(did_air, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Onsite Air Emissions Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air)[grep(pattern = "rel.year", names(coef(did_air)))]
pre.treat.coef <- pre.treat.coef[4:5]
linearHypothesis(did_air, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total point air emissions intensity
#======================================================================================================================#
did_point_air <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ e.treated +
    sw0(
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
fixest::etable(did_point_air, digits = 4, digits.stats = 4)

did_point_air <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ e.treated +
    sw0(
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
fixest::etable(did_point_air, digits = 4, digits.stats = 4)

did_point_air <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
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
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_point_air, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Onsite Point Air Emissions Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air)[grep(pattern = "rel.year", names(coef(did_point_air)))]
pre.treat.coef <- pre.treat.coef[4:5]
linearHypothesis(did_point_air, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity
#======================================================================================================================#
did_fug_air <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ e.treated +
    sw0(
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
fixest::etable(did_fug_air, digits = 4, digits.stats = 4)

did_fug_air <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ e.treated +
    sw0(
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
fixest::etable(did_fug_air, digits = 4, digits.stats = 4)

did_fug_air <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
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
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_fug_air, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Onsite Fugitive Air Emissions Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air)[grep(pattern = "rel.year", names(coef(did_fug_air)))]
pre.treat.coef <- pre.treat.coef[4:5]
linearHypothesis(did_fug_air, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "../latex/fig_did_onsite_air_emissions_int.pdf", width = 12, height = 4)
par(mfrow = c(1, 3))
fixest::iplot(did_air, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Onsite Air Emissions Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Onsite Point Air Emissions Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Onsite Fugitive Air Emissions Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Onsite: Total surface water discharge intensity
#======================================================================================================================#
did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ e.treated +
    sw0(
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
fixest::etable(did_water_disc, digits = 4, digits.stats = 4)

did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ e.treated +
    sw0(
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
fixest::etable(did_water_disc, digits = 4, digits.stats = 4)

did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
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
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_water_disc, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Onsite Surface Water Discharge Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc)[grep(pattern = "rel.year", names(coef(did_water_disc)))]
pre.treat.coef <- pre.treat.coef[4:5]
linearHypothesis(did_water_disc, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Number of receiving streams
#======================================================================================================================#
did_receiving_streams <- fixest::feols(
  total.num.receiving.streams.onsite ~ e.treated +
    sw0(
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
fixest::etable(did_receiving_streams, digits = 4, digits.stats = 4)

did_receiving_streams <- fixest::feols(
  total.num.receiving.streams.onsite ~ e.treated +
    sw0(
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
fixest::etable(did_receiving_streams, digits = 4, digits.stats = 4)

did_receiving_streams <- fixest::feols(
  total.num.receiving.streams.onsite ~
    i(rel.year, ref = c(2013, Inf)) +
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
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_receiving_streams, digits = 4, digits.stats = 4)
fixest::iplot(did_receiving_streams, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Onsite Number of Receiving Streams", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_receiving_streams)[grep(pattern = "rel.year", names(coef(did_receiving_streams)))]
pre.treat.coef <- pre.treat.coef[4:5]
linearHypothesis(did_receiving_streams, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_did_onsite_water_discharge_int.pdf", width = 10, height = 4.5)
par(mfrow = c(1, 2))
fixest::iplot(did_water_disc, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Onsite Surface Water Discharge Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_receiving_streams, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Onsite Number of Receiving Streams", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Onsite: Total land releases intensity
#======================================================================================================================#
did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ e.treated +
    sw0(
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
  l.total.land.releases.onsite.intensity ~ e.treated +
    sw0(
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
  l.total.land.releases.onsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
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
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_land_releases, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Onsite Land Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases)[grep(pattern = "rel.year", names(coef(did_land_releases)))]
pre.treat.coef <- pre.treat.coef[4:5]
linearHypothesis(did_land_releases, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total underground injection intensity
#======================================================================================================================#
did_undground_inject <- fixest::feols(
  l.total.underground.injection.onsite.intensity ~ e.treated +
    sw0(
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
fixest::etable(did_undground_inject, digits = 4, digits.stats = 4)

did_undground_inject <- fixest::feols(
  l.total.underground.injection.onsite.intensity ~ e.treated +
    sw0(
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
fixest::etable(did_undground_inject, digits = 4, digits.stats = 4)

did_undground_inject <- fixest::feols(
  l.total.underground.injection.onsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
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
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_undground_inject, digits = 4, digits.stats = 4)
fixest::iplot(did_undground_inject, xlim = c(2011, 2017), ylim = c(-0.01, 0.01), col = "blue",
              main = "Total Onsite Underground Injection Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_undground_inject)[grep(pattern = "rel.year", names(coef(did_undground_inject)))]
pre.treat.coef <- pre.treat.coef[4:5]
linearHypothesis(did_undground_inject, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total landfills intensity
#======================================================================================================================#
did_landfills <- fixest::feols(
  l.total.landfills.onsite.intensity ~ e.treated +
    sw0(
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
  l.total.landfills.onsite.intensity ~ e.treated +
    sw0(
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
  l.total.landfills.onsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
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
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_landfills, digits = 4, digits.stats = 4)
fixest::iplot(did_landfills, xlim = c(2011, 2017), ylim = c(-0.05, 0.05), col = "blue",
              main = "Total Onsite Landfills Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_landfills)[grep(pattern = "rel.year", names(coef(did_landfills)))]
pre.treat.coef <- pre.treat.coef[4:5]
linearHypothesis(did_landfills, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total releases to land treatment intensity
#======================================================================================================================#
did_release_toland <- fixest::feols(
  l.total.releases.toland.treatment.onsite.intensity ~ e.treated +
    sw0(
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
fixest::etable(did_release_toland, digits = 4, digits.stats = 4)

did_release_toland <- fixest::feols(
  l.total.releases.toland.treatment.onsite.intensity ~ e.treated +
    sw0(
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
fixest::etable(did_release_toland, digits = 4, digits.stats = 4)

did_release_toland <- fixest::feols(
  l.total.releases.toland.treatment.onsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
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
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_release_toland, digits = 4, digits.stats = 4)
fixest::iplot(did_release_toland, xlim = c(2011, 2017), ylim = c(-0.05, 0.05), col = "blue",
              main = "Total Onsite Releases to Land Treatment Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_release_toland)[grep(pattern = "rel.year", names(coef(did_release_toland)))]
pre.treat.coef <- pre.treat.coef[4:5]
linearHypothesis(did_release_toland, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface impoundment intensity
#======================================================================================================================#
did_surface_impoundment <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ e.treated +
    sw0(
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
  l.total.surface.impoundment.onsite.intensity ~ e.treated +
    sw0(
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
  l.total.surface.impoundment.onsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
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
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_surface_impoundment, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impoundment, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Onsite Surface Impoundment Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impoundment)[grep(pattern = "rel.year", names(coef(did_surface_impoundment)))]
pre.treat.coef <- pre.treat.coef[4:5]
linearHypothesis(did_surface_impoundment, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total land releases others intensity
#======================================================================================================================#
did_land_releases_others <- fixest::feols(
  l.total.land.releases.other.onsite.intensity ~ e.treated +
    sw0(
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
  l.total.land.releases.other.onsite.intensity ~ e.treated +
    sw0(
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
  l.total.land.releases.other.onsite.intensity ~
    i(rel.year, ref = c(2013, Inf)) +
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
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_land_releases_others, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_others, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Toal Onsite Land Releases Intensity, Others", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_others)[grep(pattern = "rel.year", names(coef(did_land_releases_others)))]
pre.treat.coef <- pre.treat.coef[4:5]
linearHypothesis(did_land_releases_others, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_did_total_land_releases_onsite.pdf", width = 12, height = 7)
par(mfrow = c(2, 3))
fixest::iplot(did_land_releases, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Onsite Land Releases Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_undground_inject, xlim = c(2011, 2017), ylim = c(-0.01, 0.01), col = "blue",
              main = "Total Onsite Underground Injection Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_landfills, xlim = c(2011, 2017), ylim = c(-0.05, 0.05), col = "blue",
              main = "Total Onsite Landfills Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_release_toland, xlim = c(2011, 2017), ylim = c(-0.05, 0.05), col = "blue",
              main = "Total Onsite Releases to Land Treatment Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Onsite Surface Impoundment Intensity", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_others, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Toal Onsite Land Releases Intensity, Others", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#