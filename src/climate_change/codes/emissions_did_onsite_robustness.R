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
#TODO: Construct placebo distributions as in Jakub (2024)
#======================================================================================================================#
did_total_releases_catastrophicevents <- fixest::feols(
  l.total.release.onsite.catastrophicevents.intensity ~ sunab(ch.year, year) +
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
fixest::etable(did_total_releases_catastrophicevents, agg = "ATT", digits = 4, digits.stats = 4)
fixest::etable(did_total_releases_catastrophicevents, agg = "cohort", digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_catastrophicevents, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Onsite Releases Intensity, from Catastrophic Events", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_releases_onsite_catastrophicevents_int.pdf",
    width = 8.5, height = 6)
iplot(
  list(sdid_total_releases_catastrophicevents, did_total_releases_catastrophicevents),
  xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = c("blue", "pink"),
  main = "Total Onsite Releases Intensity, from Catastrophic Events (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.0157 (0.0163)", "TWFE ATT: -0.0211 (0.019)"),
       col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
dev.off()
#======================================================================================================================#
### Alternative clustering of the SEs
#======================================================================================================================#
### Onsite: Total releases intensity
#======================================================================================================================#
did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_total_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_total_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
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

etable(did_total_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_total_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
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

etable(did_total_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_total_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_total_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_total_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_total_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_total_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_total_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_total_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_total_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_total_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_total_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_total_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_total_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_total_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_total_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_total_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_total_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_total_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_total_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_total_releases, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total Air Emissions Intensity
#======================================================================================================================#
did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_air, agg = "cohort", digits = 4, digits.stats = 4)

did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_air, agg = "cohort", digits = 4, digits.stats = 4)

did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_air, agg = "cohort", digits = 4, digits.stats = 4)

did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_air, agg = "cohort", digits = 4, digits.stats = 4)

did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_air, agg = "cohort", digits = 4, digits.stats = 4)

did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_air, agg = "cohort", digits = 4, digits.stats = 4)

did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_air, agg = "cohort", digits = 4, digits.stats = 4)

did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_air, agg = "cohort", digits = 4, digits.stats = 4)

did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_air, agg = "cohort", digits = 4, digits.stats = 4)

did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_air, agg = "cohort", digits = 4, digits.stats = 4)

did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_air, agg = "cohort", digits = 4, digits.stats = 4)

did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_air, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity
#======================================================================================================================#
did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc, agg = "cohort", digits = 4, digits.stats = 4)

did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc, agg = "cohort", digits = 4, digits.stats = 4)

did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc, agg = "cohort", digits = 4, digits.stats = 4)

did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc, agg = "cohort", digits = 4, digits.stats = 4)

did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc, agg = "cohort", digits = 4, digits.stats = 4)

did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc, agg = "cohort", digits = 4, digits.stats = 4)

did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc, agg = "cohort", digits = 4, digits.stats = 4)

did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc, agg = "cohort", digits = 4, digits.stats = 4)

did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc, agg = "cohort", digits = 4, digits.stats = 4)

did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc, agg = "cohort", digits = 4, digits.stats = 4)

did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc, agg = "cohort", digits = 4, digits.stats = 4)

did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc, agg = "cohort", digits = 4, digits.stats = 4)

did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total land releases intensity
#======================================================================================================================#
did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_land_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_land_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_land_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_land_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_land_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_land_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_land_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_land_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_land_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_land_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_land_releases, agg = "cohort", digits = 4, digits.stats = 4)

did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_land_releases, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Removing highest emitting states >= 5million lbs
### They include: treated (MN and SD) and control states (IA, IL, PA, and WI)
#======================================================================================================================#
did_total_releases_nohigh_emitstates <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
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
    csw(,
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>% filter(!facility.state %in% c("MN", "SD") |
                            !treated.match %in% c("MN", "SD"))
  ,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(did_total_releases_nohigh_emitstates, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_total_releases_nohigh_emitstates, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total air emissions intensity
#======================================================================================================================#
did_air_nohigh_emitstates <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
    csw(,
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>% filter(!facility.state %in% c("MN", "SD") |
                            !treated.match %in% c("MN", "SD"))
  ,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(did_air_nohigh_emitstates, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_air_nohigh_emitstates, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total point air emissions intensity
#======================================================================================================================#
did_point_air_nohigh_emitstates <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
    csw(,
      year,
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>% filter(!facility.state %in% c("MN", "SD") |
                            !treated.match %in% c("MN", "SD"))
  ,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(did_point_air_nohigh_emitstates, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_point_air_nohigh_emitstates, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity
#======================================================================================================================#
did_fug_air_nohigh_emitstates <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
    csw(,
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>% filter(!facility.state %in% c("MN", "SD") |
                            !treated.match %in% c("MN", "SD"))
  ,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(did_fug_air_nohigh_emitstates, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_fug_air_nohigh_emitstates, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity
#======================================================================================================================#
did_water_disc_nohigh_emitstates <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
    csw(,
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>% filter(!facility.state %in% c("MN", "SD") |
                            !treated.match %in% c("MN", "SD"))
  ,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(did_water_disc_nohigh_emitstates, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc_nohigh_emitstates, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total land releases intensity
#======================================================================================================================#
did_land_releases_nohigh_emitstates <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
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
    csw(,
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>% filter(!facility.state %in% c("MN", "SD") |
                            !treated.match %in% c("MN", "SD"))
  ,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(did_land_releases_nohigh_emitstates, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_land_releases_nohigh_emitstates, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Removing highest emitting industries >= 5million lbs
### They include: treated (chemical, food, and transport and equipment manufacturing)
#======================================================================================================================#
did_total_releases_nohigh_emitindustry <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
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
    csw(,
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!industry.name %in%
      c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(did_total_releases_nohigh_emitindustry, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_total_releases_nohigh_emitindustry, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total air emissions intensity
#======================================================================================================================#
did_air_nohigh_emitindustry <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
    csw(,
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!industry.name %in%
      c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(did_air_nohigh_emitindustry, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_air_nohigh_emitindustry, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total point air emissions intensity
#======================================================================================================================#
did_point_air_nohigh_emitindustry <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
    csw(,
      year,
      facility.id.fe,
      border.county.fe,
      chemical.id.fe,
      chemical.year.fe
    )
  ,
  data = triQc %>%
    filter(!industry.name %in%
      c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(did_point_air_nohigh_emitindustry, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_point_air_nohigh_emitindustry, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity
#======================================================================================================================#
did_fug_air <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_fug_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_fug_air, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity
#======================================================================================================================#
did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_water_disc, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total land releases intensity
#======================================================================================================================#
did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
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
etable(did_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(did_land_releases, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#