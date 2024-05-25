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
#======================================================================================================================#
### Onsite Placebo: Total releases intensity, from catastrophic events
#======================================================================================================================#
did_total_releases_catastrophicevents <- fixest::feols(
  l.total.release.onsite.catastrophicevents.intensity ~ e.treated +
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_total_releases_catastrophicevents, digits = 4, digits.stats = 4)

did_total_releases_catastrophicevents <- fixest::feols(
  l.total.release.onsite.catastrophicevents.intensity ~
    i(rel.year, ref = c(-1, Inf)) +
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
        facility.id.fe +
        border.county.fe +
        chemical.id.fe +
        chemical.year.fe
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(did_total_releases_catastrophicevents, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_catastrophicevents, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Onsite Releases Intensity, from Catastrophic Events", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_catastrophicevents)[grep(pattern = "rel.year", names(coef(did_total_releases_catastrophicevents)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(did_total_releases_catastrophicevents, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_did_total_releases_onsite_catastrophicevents_int.pdf",
    width = 6.5, height = 5)
# par(mfrow = c(1, 2))
fixest::iplot(did_total_releases_catastrophicevents, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Onsite Releases Intensity, from Catastrophic Events", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Alternative clustering of the SEs
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
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.id,
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.zipcode,
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~fips.code,
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~naics.code,
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~chemical.id,
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state,
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(facility.state, chemical.id),
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(facility.state, naics.code),
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code),
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, facility.state),
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(facility.id, facility.state),
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(naics.code, facility.state),
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total Air Emissions Intensity
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
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.id,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.zipcode,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~fips.code,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~naics.code,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~chemical.id,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(facility.id, chemical.id),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(facility.id, naics.code),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, facility.state),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(facility.id, facility.state),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(naics.code, facility.state),
)
fixest::etable(did_air, digits = 4, digits.stats = 4)
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
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.id,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.zipcode,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~fips.code,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~naics.code,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~chemical.id,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(facility.id, chemical.id),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(facility.id, naics.code),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~chemical.id,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, facility.state),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(facility.id, facility.state),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(naics.code, facility.state),
)
fixest::etable(did_water_disc, digits = 4, digits.stats = 4)
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
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.id,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.zipcode,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~fips.code,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~naics.code,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~chemical.id,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state,
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(facility.id, chemical.id),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(facility.id, naics.code),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(chemical.id, facility.state),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(facility.id, facility.state),
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(naics.code, facility.state),
)
fixest::etable(did_land_releases, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Removing highest emitting states >= 5million lbs
### They include: treated (MN and SD) and control states (IA, IL, PA, and WI)
#======================================================================================================================#
did_total_releases_nohigh_emitstates <- fixest::feols(
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!facility.state %in% c("MN", "SD")) %>%
    filter(!treated.match %in% c("MN", "SD"))
  ,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_total_releases_nohigh_emitstates, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total air emissions intensity
#======================================================================================================================#
did_air_nohigh_emitstates <- fixest::feols(
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!facility.state %in% c("MN", "SD")) %>%
    filter(!treated.match %in% c("MN", "SD"))
  ,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_air_nohigh_emitstates, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total point air emissions intensity
#======================================================================================================================#
did_point_air_nohigh_emitstates <- fixest::feols(
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
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!facility.state %in% c("MN", "SD")) %>%
    filter(!treated.match %in% c("MN", "SD"))
  ,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_point_air_nohigh_emitstates, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity
#======================================================================================================================#
did_fug_air_nohigh_emitstates <- fixest::feols(
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!facility.state %in% c("MN", "SD")) %>%
    filter(!treated.match %in% c("MN", "SD"))
  ,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_fug_air_nohigh_emitstates, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity
#======================================================================================================================#
did_water_disc_nohigh_emitstates <- fixest::feols(
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!facility.state %in% c("MN", "SD")) %>%
    filter(!treated.match %in% c("MN", "SD"))
  ,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_water_disc_nohigh_emitstates, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total land releases intensity
#======================================================================================================================#
did_land_releases_nohigh_emitstates <- fixest::feols(
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!facility.state %in% c("MN", "SD")) %>%
    filter(!treated.match %in% c("MN", "SD"))
  ,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_land_releases_nohigh_emitstates, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Removing highest emitting industries >= 5million lbs
### They include: treated (chemical, food, and transport and equipment manufacturing)
#======================================================================================================================#
did_total_releases_nohigh_emitindustry <- fixest::feols(
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_total_releases_nohigh_emitindustry, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total air emissions intensity
#======================================================================================================================#
did_air_nohigh_emitindustry <- fixest::feols(
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_air_nohigh_emitindustry, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total point air emissions intensity
#======================================================================================================================#
did_point_air_nohigh_emitindustry <- fixest::feols(
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
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_point_air_nohigh_emitindustry, digits = 4, digits.stats = 4)
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
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_fug_air, digits = 4, digits.stats = 4)
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
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_water_disc, digits = 4, digits.stats = 4)
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
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(did_land_releases, digits = 4, digits.stats = 4)
#======================================================================================================================#