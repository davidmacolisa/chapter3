#======================================================================================================================#
### Unforeseen Minimum Wage Consequences---Case Study
### 30 October 2023
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

sort(unique(triQc$year))
sort(unique(triQc$rel.year))

triQc %>%
  group_by(industry.name) %>%
  summarise(industry.emissions.group = sum(total.releases.onsite.intensity)) %>%
  arrange(desc(industry.emissions.group)) %>%
  print(n = nrow(.))
#======================================================================================================================#
### Onsite: Total releases intensity
#======================================================================================================================#
sdid_total_releases <- fixest::feols(
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
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc %>% filter(# more than 1k lbs
    industry.name %in% c("Food Manufacturing", "Chemical Manufacturing",
                         "Leather and Allied Product Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(sdid_total_releases, agg = "cohort", digits = 4, digits.stats = 4)
iplot(sdid_total_releases, xlim = c(-3, 3), ylim = c(-0.3, 0.9))

sdid_total_releases <- fixest::feols(
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
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe
  ,
  data = triQc %>% filter(# more than 1k lbs
    !industry.name %in% c("Food Manufacturing", "Chemical Manufacturing",
                         "Leather and Allied Product Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(sdid_total_releases, agg = "cohort", digits = 4, digits.stats = 4)
iplot(sdid_total_releases, xlim = c(-3, 3), ylim = c(-0.3, 0.3))
#======================================================================================================================#
### Onsite: Total air emissions intensity
#======================================================================================================================#
sdid_air <- feols(
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
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe,
  data = triQc %>% filter(# more than 1k lbs
    industry.name %in% c("Food Manufacturing", "Chemical Manufacturing",
                         "Leather and Allied Product Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(sdid_air, agg = "cohort", digits = 4, digits.stats = 4)
iplot(sdid_air, xlim = c(-3, 3), ylim = c(-0.3, 0.3))

sdid_air <- feols(
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
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe,
  data = triQc %>% filter(# more than 1k lbs
    !industry.name %in% c("Food Manufacturing", "Chemical Manufacturing",
                         "Leather and Allied Product Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(sdid_air, agg = "cohort", digits = 4, digits.stats = 4)
iplot(sdid_air, xlim = c(-3, 3), ylim = c(-0.3, 0.3))
#======================================================================================================================#
### Onsite: Total point air emissions intensity
#======================================================================================================================#
sdid_point_air <- feols(
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
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe,
  cluster = ~c(chemical.id, naics.code, facility.state),
  data = triQc %>% filter(# more than 1k lbs
    industry.name %in% c("Food Manufacturing", "Chemical Manufacturing",
                         "Leather and Allied Product Manufacturing")),
)
etable(sdid_point_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(sdid_point_air, agg = "cohort", digits = 4, digits.stats = 4)

sdid_point_air <- feols(
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
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe,
  cluster = ~c(chemical.id, naics.code, facility.state),
  data = triQc %>% filter(# more than 1k lbs
    !industry.name %in% c("Food Manufacturing", "Chemical Manufacturing",
                         "Leather and Allied Product Manufacturing")),
)
etable(sdid_point_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(sdid_point_air, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity
#======================================================================================================================#
sdid_fug_air <- feols(
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
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe,
  cluster = ~c(chemical.id, naics.code, facility.state),
  data = triQc %>% filter(# more than 1k lbs
    industry.name %in% c("Food Manufacturing", "Chemical Manufacturing",
                         "Leather and Allied Product Manufacturing")),
)
etable(sdid_fug_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(sdid_fug_air, agg = "cohort", digits = 4, digits.stats = 4)

sdid_fug_air <- feols(
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
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe,
  cluster = ~c(chemical.id, naics.code, facility.state),
  data = triQc %>% filter(# more than 1k lbs
    !industry.name %in% c("Food Manufacturing", "Chemical Manufacturing",
                         "Leather and Allied Product Manufacturing")),
)
etable(sdid_fug_air, agg = "ATT", digits = 4, digits.stats = 4)
etable(sdid_fug_air, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity
#======================================================================================================================#
sdid_water_disc <- feols(
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
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe,
  cluster = ~c(chemical.id, naics.code, facility.state),
  data = triQc %>% filter(# more than 1k lbs
    industry.name %in% c("Food Manufacturing", "Chemical Manufacturing")),
)
etable(sdid_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(sdid_water_disc, agg = "cohort", digits = 4, digits.stats = 4)

sdid_water_disc <- feols(
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
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe,
  cluster = ~c(chemical.id, naics.code, facility.state),
  data = triQc %>% filter(# more than 1k lbs
    !industry.name %in% c("Food Manufacturing", "Chemical Manufacturing")),
)
etable(sdid_water_disc, agg = "ATT", digits = 4, digits.stats = 4)
etable(sdid_water_disc, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total land releases intensity
#======================================================================================================================#
sdid_land_releases <- feols(
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
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe,
  cluster = ~c(chemical.id, naics.code, facility.state),
  data = triQc %>% filter(# more than 1k lbs
    industry.name %in% c("Food Manufacturing", "Chemical Manufacturing",
                         "Wood Product Manufacturing")),
)
etable(sdid_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(sdid_land_releases, agg = "cohort", digits = 4, digits.stats = 4)

sdid_land_releases <- feols(
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
    year +
      facility.id.fe +
      border.county.fe +
      chemical.id.fe +
      chemical.year.fe,
  cluster = ~c(chemical.id, naics.code, facility.state),
  data = triQc %>% filter(# more than 1k lbs
    !industry.name %in% c("Food Manufacturing", "Chemical Manufacturing",
                         "Wood Product Manufacturing")),
)
etable(sdid_land_releases, agg = "ATT", digits = 4, digits.stats = 4)
etable(sdid_land_releases, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#
### Onsite: Total surface impoundment intensity
#======================================================================================================================#
sdid_surface_impoundment <- feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
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
      chemical.year.fe,
  cluster = ~c(chemical.id, naics.code, facility.state),
  data = triQc %>% filter(# more than 1k lbs
    industry.name %in% c("Food Manufacturing", "Chemical Manufacturing",
                         "Wood Product Manufacturing")),
)
etable(sdid_surface_impoundment, agg = "ATT", digits = 4, digits.stats = 4)
etable(sdid_surface_impoundment, agg = "cohort", digits = 4, digits.stats = 4)

sdid_surface_impoundment <- feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
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
      chemical.year.fe,
  cluster = ~c(chemical.id, naics.code, facility.state),
  data = triQc %>% filter(# more than 1k lbs
    !industry.name %in% c("Food Manufacturing", "Chemical Manufacturing",
                         "Wood Product Manufacturing")),
)
etable(sdid_surface_impoundment, agg = "ATT", digits = 4, digits.stats = 4)
etable(sdid_surface_impoundment, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#