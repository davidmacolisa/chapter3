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
### Triple Difference
#======================================================================================================================#
### Onsite Releases Intensity
#======================================================================================================================#
### Economic Growth Patterns: GDP
### Higher GDP counties are above the median GDP, and low GDP counties are below the median GDP.
#======================================================================================================================#
# median_gdp <- median(triQc$gdp)
# triQc$high.gdp <- ifelse(triQc$gdp > median_gdp, yes = 1, no = 0)
gdp_50th_percentile <- quantile(triQc$gdp, probs = 0.5) # same as above
triQc$high.gdp <- ifelse(triQc$gdp > gdp_50th_percentile, yes = 1, no = 0)
prop.table(table(triQc$high.gdp)) * 100
#======================================================================================================================#
### Onsite: Total releases intensity
#======================================================================================================================#
did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ treated:post:high.gdp +
    treated:post +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

did_total_releases_hgdp <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(treated * year * high.gdp, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_total_releases_hgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_hgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_hgdp)[grep(pattern = "treated", names(coef(did_total_releases_hgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_total_releases_hgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_total_releases_lgdp <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post:high.gdp +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_total_releases_lgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_lgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_lgdp)[grep(pattern = "treated", names(coef(did_total_releases_lgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_total_releases_lgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total air emissions intensity
#======================================================================================================================#
did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ treated:post:high.gdp +
    treated:post +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_air, digits = 4, digits.stats = 4)

did_air_hgdp <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year * high.gdp, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_air_hgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_air_hgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Air Emissions Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_hgdp)[grep(pattern = "treated", names(coef(did_air_hgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_hgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_air_lgdp <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post:high.gdp +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_air_lgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_air_lgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Air Emissions Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_lgdp)[grep(pattern = "treated", names(coef(did_air_lgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_lgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total point air emissions intensity
#======================================================================================================================#
did_point_air <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ treated:post:high.gdp +
    treated:post +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_point_air, digits = 4, digits.stats = 4)

did_point_air_hgdp <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(treated * year * high.gdp, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_point_air_hgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air_hgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air_hgdp)[grep(pattern = "treated", names(coef(did_point_air_hgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_point_air_hgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_point_air_lgdp <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post:high.gdp +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_point_air_lgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air_lgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air_lgdp)[grep(pattern = "treated", names(coef(did_point_air_lgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_point_air_lgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity
#======================================================================================================================#
did_fug_air <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ treated:post:high.gdp +
    treated:post +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_fug_air, digits = 4, digits.stats = 4)

did_fug_air_hgdp <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(treated * year * high.gdp, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_fug_air_hgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air_hgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air_hgdp)[grep(pattern = "treated", names(coef(did_fug_air_hgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_fug_air_hgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_fug_air_lgdp <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post:high.gdp +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_fug_air_lgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air_lgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air_lgdp)[grep(pattern = "treated", names(coef(did_fug_air_lgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_fug_air_lgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface water discharge intensity
#======================================================================================================================#
did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ treated:post:high.gdp +
    treated:post +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_water_disc, digits = 4, digits.stats = 4)

did_water_disc_hgdp <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year * high.gdp, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_water_disc_hgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc_hgdp, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Onsite Surface Water Discharge Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc_hgdp)[grep(pattern = "treated", names(coef(did_water_disc_hgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_water_disc_hgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_water_disc_lgdp <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post:high.gdp +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_water_disc_lgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc_lgdp, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Onsite Surface Water Discharge Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc_lgdp)[grep(pattern = "treated", names(coef(did_water_disc_lgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_water_disc_lgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total land releases intensity
#======================================================================================================================#
did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ treated:post:high.gdp +
    treated:post +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_land_releases, digits = 4, digits.stats = 4)

did_land_releases_hgdp <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year * high.gdp, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_land_releases_hgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_hgdp, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Onsite Land Releases Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_hgdp)[grep(pattern = "treated", names(coef(did_land_releases_hgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_hgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_land_releases_lgdp <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post:high.gdp +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_land_releases_lgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_lgdp, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Onsite Land Releases Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_lgdp)[grep(pattern = "treated", names(coef(did_land_releases_lgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_lgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total land releases intensity: Surface impoundment
#======================================================================================================================#
did_surface_impound <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ treated:post:high.gdp +
    treated:post +
    treated:high.gdp +
    post:high.gdp +
    treated +
    high.gdp +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_surface_impound, digits = 4, digits.stats = 4)

did_surface_impound_hgdp <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~
    i(treated * year * high.gdp, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_surface_impound_hgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impound_hgdp, xlim = c(2011, 2017), ylim = c(-0.02, 0.02), col = "blue",
              main = "Total Surface Impoundment Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_hgdp)[grep(pattern = "treated", names(coef(did_land_releases_hgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_hgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_surface_impound_lgdp <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post:high.gdp +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_surface_impound_lgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impound_lgdp, xlim = c(2011, 2017), ylim = c(-0.02, 0.02), col = "blue",
              main = "Total Surface Impoundment Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impound_lgdp)[grep(pattern = "treated", names(coef(did_surface_impound_lgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_surface_impound_lgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_cs_did_total_onsite_releases_int_GDP.pdf", width = 20, height = 15)
par(mfrow = c(4, 4))
fixest::iplot(did_total_releases_hgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_total_releases_lgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_hgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Air Emissions Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_lgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Air Emissions Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air_hgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air_lgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air_hgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air_lgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_water_disc_hgdp, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Onsite Surface Water Discharge Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_water_disc_lgdp, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Onsite Surface Water Discharge Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_hgdp, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Onsite Land Releases Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_lgdp, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Onsite Land Releases Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impound_hgdp, xlim = c(2011, 2017), ylim = c(-0.02, 0.02), col = "blue",
              main = "Total Surface Impoundment Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impound_lgdp, xlim = c(2011, 2017), ylim = c(-0.02, 0.02), col = "blue",
              main = "Total Surface Impoundment Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Is the MW Policy potentially Carcinogenic?
#======================================================================================================================#
### Onsite: Total releases intensity (carcinonogenic chemicals)
#======================================================================================================================#
did_total_releases_carcinogenic <- fixest::feols(
  l.total.releases.onsite.intensity ~ treated:post:carcinogenic.chems +
    treated:post +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_total_releases_carcinogenic, digits = 4, digits.stats = 4)

did_total_releases_carcinogenic <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(treated * year * carcinogenic.chems, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_total_releases_carcinogenic, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_carcinogenic, xlim = c(2011, 2017), ylim = c(-1.2, 1.2), col = "blue",
              main = "Total Onsite Releases Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_carcinogenic)[grep(pattern = "treated", names(coef(did_total_releases_carcinogenic)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_total_releases_carcinogenic, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_total_releases_noncarcinogenic <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post:carcinogenic.chems +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_total_releases_noncarcinogenic, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_noncarcinogenic, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_noncarcinogenic)[grep(pattern = "treated", names(coef(did_total_releases_noncarcinogenic)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_total_releases_noncarcinogenic, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total air emissions intensity (carcinogenic chemicals)
#======================================================================================================================#
did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ treated:post:carcinogenic.chems +
    treated:post +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_air, digits = 4, digits.stats = 4)

did_air_carcinogenic <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year * carcinogenic.chems, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_air_carcinogenic, digits = 4, digits.stats = 4)
fixest::iplot(did_air_carcinogenic, xlim = c(2011, 2017), ylim = c(-1.2, 1.3), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_carcinogenic)[grep(pattern = "treated", names(coef(did_air_carcinogenic)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_carcinogenic, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_air_noncarcinogenic <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post:carcinogenic.chems +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_air_noncarcinogenic, digits = 4, digits.stats = 4)
fixest::iplot(did_air_noncarcinogenic, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_noncarcinogenic)[grep(pattern = "treated", names(coef(did_air_noncarcinogenic)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_noncarcinogenic, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total point air emissions intensity (carcinogenic chemicals)
#======================================================================================================================#
did_point_air <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ treated:post:carcinogenic.chems +
    treated:post +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_point_air, digits = 4, digits.stats = 4)

did_point_air_carcinogenic <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(treated * year * carcinogenic.chems, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_point_air_carcinogenic, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air_carcinogenic, xlim = c(2011, 2017), ylim = c(-1.7, 1.5), col = "blue",
              main = "Total Point Air Emissions Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air_carcinogenic)[grep(pattern = "treated", names(coef(did_point_air_carcinogenic)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_point_air_carcinogenic, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_point_air_noncarcinogenic <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post:carcinogenic.chems +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_point_air_noncarcinogenic, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air_noncarcinogenic, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air_noncarcinogenic)[grep(pattern = "treated", names(coef(did_point_air_noncarcinogenic)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_point_air_noncarcinogenic, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (carcinogenic chemicals)
#======================================================================================================================#
did_fug_air <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ treated:post:carcinogenic.chems +
    treated:post +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_fug_air, digits = 4, digits.stats = 4)

did_fug_air_carcinogenic <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(treated * year * carcinogenic.chems, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_fug_air_carcinogenic, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air_carcinogenic, xlim = c(2011, 2017), ylim = c(-1.2, 1.2), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air_carcinogenic)[grep(pattern = "treated", names(coef(did_fug_air_carcinogenic)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_fug_air_carcinogenic, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_fug_air_noncarcinogenic <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post:carcinogenic.chems +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_fug_air_noncarcinogenic, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air_noncarcinogenic, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air_noncarcinogenic)[grep(pattern = "treated", names(coef(did_fug_air_noncarcinogenic)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_fug_air_noncarcinogenic, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (carcinogenic chemicals)
#======================================================================================================================#
did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ treated:post:carcinogenic.chems +
    treated:post +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_water_disc, digits = 4, digits.stats = 4)

did_water_disc_carcinogenic <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year * carcinogenic.chems, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_water_disc_carcinogenic, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc_carcinogenic, xlim = c(2011, 2017), ylim = c(-1.2, 0.9), col = "blue",
              main = "Total Surface Water Discharge Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc_carcinogenic)[grep(pattern = "treated", names(coef(did_water_disc_carcinogenic)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_water_disc_carcinogenic, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_water_disc_noncarcinogenic <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post:carcinogenic.chems +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_water_disc_noncarcinogenic, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc_noncarcinogenic, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc_noncarcinogenic)[grep(pattern = "treated", names(coef(did_water_disc_noncarcinogenic)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_water_disc_noncarcinogenic, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total land releases intensity (carcinogenic chemicals)
#======================================================================================================================#
did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ treated:post:carcinogenic.chems +
    treated:post +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_land_releases, digits = 4, digits.stats = 4)

did_land_releases_carcinogenic <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year * carcinogenic.chems, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_land_releases_carcinogenic, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_carcinogenic, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Land Releases Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_carcinogenic)[grep(pattern = "treated", names(coef(did_land_releases_carcinogenic)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_carcinogenic, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_land_releases_noncarcinogenic <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post:carcinogenic.chems +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_land_releases_noncarcinogenic, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_noncarcinogenic, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Land Releases Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_noncarcinogenic)[grep(pattern = "treated", names(coef(did_land_releases_noncarcinogenic)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_noncarcinogenic, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (carcinogenic chemicals)
#======================================================================================================================#
did_surface_impoundment <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ treated:post:carcinogenic.chems +
    treated:post +
    treated:carcinogenic.chems +
    post:carcinogenic.chems +
    treated +
    carcinogenic.chems +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_surface_impoundment, digits = 4, digits.stats = 4)

did_surface_impoundment_carcinogenic <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~
    i(treated * year * carcinogenic.chems, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_surface_impoundment_carcinogenic, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impoundment_carcinogenic, xlim = c(2011, 2017), ylim = c(-0.05, 0.05), col = "blue",
              main = "Total Surface Impoundment Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impoundment_carcinogenic)[grep(pattern = "treated", names(coef(did_surface_impoundment_carcinogenic)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_surface_impoundment_carcinogenic, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_surface_impoundment_noncarcinogenic <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post:carcinogenic.chems +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_surface_impoundment_noncarcinogenic, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impoundment_noncarcinogenic, xlim = c(2011, 2017), ylim = c(-0.03, 0.03), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impoundment_noncarcinogenic)[grep(pattern = "treated", names(coef(did_surface_impoundment_noncarcinogenic)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_surface_impoundment_noncarcinogenic, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_cs_did_total_onsite_releases_int_carcinogens.pdf", width = 20, height = 15)
par(mfrow = c(4, 4))
fixest::iplot(did_total_releases_carcinogenic, xlim = c(2011, 2017), ylim = c(-1.2, 1.2), col = "blue",
              main = "Total Onsite Releases Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_total_releases_noncarcinogenic, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_carcinogenic, xlim = c(2011, 2017), ylim = c(-1.3, 1.3), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_noncarcinogenic, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air_carcinogenic, xlim = c(2011, 2017), ylim = c(-1.7, 1.7), col = "blue",
              main = "Total Point Air Emissions Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air_noncarcinogenic, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air_carcinogenic, xlim = c(2011, 2017), ylim = c(-1.2, 1.2), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air_noncarcinogenic, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_water_disc_carcinogenic, xlim = c(2011, 2017), ylim = c(-1.2, 1.2), col = "blue",
              main = "Total Surface Water Discharge Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_water_disc_noncarcinogenic, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_carcinogenic, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Land Releases Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_noncarcinogenic, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Land Releases Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment_carcinogenic, xlim = c(2011, 2017), ylim = c(-0.05, 0.05), col = "blue",
              main = "Total Surface Impoundment Intensity, Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment_noncarcinogenic, xlim = c(2011, 2017), ylim = c(-0.02, 0.02), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-Carcinogens", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Does the MW Policy affect emissions intensity of the clean air act regulated chemicals?
#======================================================================================================================#
### Onsite: Total releases intensity (clean air act regulated chemicals)
#======================================================================================================================#
did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ treated:post:clean.air.act.chems +
    treated:post +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

did_total_releases_caa <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(treated * year * clean.air.act.chems, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_total_releases_caa, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_caa, xlim = c(2011, 2017), ylim = c(-1.5, 1.5), col = "blue",
              main = "Total Onsite Releases Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_caa)[grep(pattern = "treated", names(coef(did_total_releases_caa)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_total_releases_caa, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_total_releases_noncaa <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_total_releases_noncaa, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_noncaa, xlim = c(2011, 2017), ylim = c(-50, 50), col = "blue",
              main = "Total Onsite Releases Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_noncaa)[grep(pattern = "treated", names(coef(did_total_releases_noncaa)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_total_releases_noncaa, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total air emissions intensity (clean air act regulated chemicals)
#======================================================================================================================#
did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ treated:post:clean.air.act.chems +
    treated:post +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id)
)
fixest::etable(did_air, digits = 4, digits.stats = 4)

did_air_caa <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year * clean.air.act.chems, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_air_caa, digits = 4, digits.stats = 4)
fixest::iplot(did_air_caa, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Air Emissions Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_caa)[grep(pattern = "treated", names(coef(did_air_caa)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_caa, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_air_noncaa <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
  data = triQc %>%
    filter(industry.name %in% c("Chemical Manufacturing", "Food Manufacturing", "Transportation and Equipment Manufacturing")),
  cluster = ~c(chemical.id, naics.code, facility.state.id),
)
fixest::etable(did_air_noncaa, digits = 4, digits.stats = 4)
fixest::iplot(did_air_noncaa, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_noncaa)[grep(pattern = "treated", names(coef(did_air_noncaa)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_noncaa, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total point air emissions intensity (clean air act regulated chemicals)
#======================================================================================================================#
did_point_air <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ treated:post:clean.air.act.chems +
    treated:post +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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

did_point_air_caa <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(treated * year * clean.air.act.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_point_air_caa, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air_caa, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air_caa)[grep(pattern = "treated", names(coef(did_point_air_caa)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_point_air_caa, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_point_air_noncaa <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_point_air_noncaa, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air_noncaa, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air_noncaa)[grep(pattern = "treated", names(coef(did_point_air_noncaa)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_point_air_noncaa, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (clean air act regulated chemicals)
#======================================================================================================================#
did_fug_air <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ treated:post:clean.air.act.chems +
    treated:post +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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

did_fug_air_caa <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(treated * year * clean.air.act.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_fug_air_caa, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air_caa, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air_caa)[grep(pattern = "treated", names(coef(did_fug_air_caa)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_fug_air_caa, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_fug_air_noncaa <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_fug_air_noncaa, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air_noncaa, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air_noncaa)[grep(pattern = "treated", names(coef(did_fug_air_noncaa)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_fug_air_noncaa, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (clean air act regulated chemicals)
#======================================================================================================================#
did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ treated:post:clean.air.act.chems +
    treated:post +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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

did_water_disc_caa <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year * clean.air.act.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_water_disc_caa, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc_caa, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Surface Water Discharge Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc_caa)[grep(pattern = "treated", names(coef(did_water_disc_caa)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_water_disc_caa, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_water_disc_noncaa <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_water_disc_noncaa, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc_noncaa, xlim = c(2011, 2017), ylim = c(-0.35, 0.35), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc_noncaa)[grep(pattern = "treated", names(coef(did_fug_air_noncaa)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_water_disc_noncaa, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total land releases intensity (clean air act regulated chemicals)
#======================================================================================================================#
did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ treated:post:clean.air.act.chems +
    treated:post +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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

did_land_releases_caa <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year * clean.air.act.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_land_releases_caa, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_caa, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Land Releases Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_caa)[grep(pattern = "treated", names(coef(did_land_releases_caa)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_caa, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_land_releases_noncaa <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_land_releases_noncaa, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_noncaa, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Land Releases Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_noncaa)[grep(pattern = "treated", names(coef(did_land_releases_noncaa)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_noncaa, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (clean air act regulated chemicals)
#======================================================================================================================#
did_surface_impoundment <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ treated:post:clean.air.act.chems +
    treated:post +
    treated:clean.air.act.chems +
    post:clean.air.act.chems +
    treated +
    clean.air.act.chems +
    post +
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

did_surface_impoundment_caa <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~
    i(treated * year * clean.air.act.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_surface_impoundment_caa, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impoundment_caa, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impoundment_caa)[grep(pattern = "treated", names(coef(did_surface_impoundment_caa)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_surface_impoundment_caa, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_surface_impoundment_noncaa <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_surface_impoundment_noncaa, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impoundment_noncaa, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impoundment_noncaa)[grep(pattern = "treated", names(coef(did_surface_impoundment_noncaa)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_surface_impoundment_noncaa, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_did_total_onsite_releases_int_caa.pdf", width = 20, height = 15)
par(mfrow = c(4, 4))
fixest::iplot(did_total_releases_caa, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_total_releases_noncaa, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Releases Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_caa, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Air Emissions Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_noncaa, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air_caa, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air_noncaa, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air_caa, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air_noncaa, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_water_disc_caa, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Surface Water Discharge Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_water_disc_noncaa, xlim = c(2011, 2017), ylim = c(-0.35, 0.35), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_caa, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Land Releases Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_noncaa, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Land Releases Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment_caa, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment_noncaa, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-CAA", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Does the MW Policy affect emissions intensity of the Hazaradous Air Pollutants?
### TRI---General EPCRA Section 313 chemical
#======================================================================================================================#
### Onsite: Total releases intensity (Hazaradous Air Pollutants)
#======================================================================================================================#
did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ treated:post:hap.chems +
    treated:post +
    treated:hap.chems +
    post:hap.chems +
    treated +
    hap.chems +
    post +
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
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

did_total_releases_hap <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(treated * year * hap.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_total_releases_hap, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_hap, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_hap)[grep(pattern = "treated", names(coef(did_total_releases_hap)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_total_releases_hap, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_total_releases_nonhap <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_total_releases_nonhap, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_nonhap, xlim = c(2011, 2017), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total Onsite Releases Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_nonhap)[grep(pattern = "treated", names(coef(did_total_releases_nonhap)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_total_releases_nonhap, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total air emissions intensity (Hazaradous Air Pollutants)
#======================================================================================================================#
did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ treated:post:hap.chems +
    treated:post +
    treated:hap.chems +
    post:hap.chems +
    treated +
    hap.chems +
    post +
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
fixest::etable(did_air, digits = 4, digits.stats = 4)

did_air_hap <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year * hap.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_air_hap, digits = 4, digits.stats = 4)
fixest::iplot(did_air_hap, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Air Emissions Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_hap)[grep(pattern = "treated", names(coef(did_air_hap)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_hap, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_air_nonhap <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_air_nonhap, digits = 4, digits.stats = 4)
fixest::iplot(did_air_nonhap, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_nonhap)[grep(pattern = "treated", names(coef(did_air_nonhap)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_nonhap, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total point air emissions intensity (Hazaradous Air Pollutants)
#======================================================================================================================#
did_point_air <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ treated:post:hap.chems +
    treated:post +
    treated:hap.chems +
    post:hap.chems +
    treated +
    hap.chems +
    post +
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

did_point_air_hap <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(treated * year * hap.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_point_air_hap, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air_hap, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air_hap)[grep(pattern = "treated", names(coef(did_point_air_hap)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_point_air_hap, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_point_air_nonhap <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_point_air_nonhap, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air_nonhap, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air_nonhap)[grep(pattern = "treated", names(coef(did_point_air_nonhap)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_point_air_nonhap, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (Hazaradous Air Pollutants)
#======================================================================================================================#
did_fug_air <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ treated:post:hap.chems +
    treated:post +
    treated:hap.chems +
    post:hap.chems +
    treated +
    hap.chems +
    post +
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

did_fug_air_hap <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(treated * year * hap.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_fug_air_hap, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air_hap, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air_hap)[grep(pattern = "treated", names(coef(did_fug_air_hap)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_fug_air_hap, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_fug_air_nonhap <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_fug_air_nonhap, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air_nonhap, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air_nonhap)[grep(pattern = "treated", names(coef(did_fug_air_nonhap)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_fug_air_nonhap, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (Hazaradous Air Pollutants)
#======================================================================================================================#
did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ treated:post:hap.chems +
    treated:post +
    treated:hap.chems +
    post:hap.chems +
    treated +
    hap.chems +
    post +
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

did_water_disc_hap <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year * hap.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_water_disc_hap, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc_hap, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Surface Water Discharge Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc_hap)[grep(pattern = "treated", names(coef(did_water_disc_hap)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_water_disc_hap, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_water_disc_nonhap <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_water_disc_nonhap, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc_nonhap, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc_nonhap)[grep(pattern = "treated", names(coef(did_water_disc_nonhap)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_water_disc_nonhap, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total land releases intensity (Hazaradous Air Pollutants)
#======================================================================================================================#
did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ treated:post:hap.chems +
    treated:post +
    treated:hap.chems +
    post:hap.chems +
    treated +
    hap.chems +
    post +
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

did_land_releases_hap <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year * hap.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_land_releases_hap, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_hap, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Land Releases Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_hap)[grep(pattern = "treated", names(coef(did_land_releases_hap)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_hap, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_land_releases_nonhap <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_land_releases_nonhap, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_nonhap, xlim = c(2011, 2017), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_nonhap)[grep(pattern = "treated", names(coef(did_land_releases_nonhap)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_nonhap, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (Hazaradous Air Pollutants)
#======================================================================================================================#
did_surface_impoundment <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ treated:post:hap.chems +
    treated:post +
    treated:hap.chems +
    post:hap.chems +
    treated +
    hap.chems +
    post +
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

did_surface_impoundment_hap <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~
    i(treated * year * hap.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_surface_impoundment_hap, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impoundment_hap, xlim = c(2011, 2017), ylim = c(-0.08, 0.08), col = "blue",
              main = "Total Surface Impoundment Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impoundment_hap)[grep(pattern = "treated", names(coef(did_surface_impoundment_hap)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_surface_impoundment_hap, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_surface_impoundment_nonhap <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_surface_impoundment_nonhap, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impoundment_nonhap, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impoundment_nonhap)[grep(pattern = "treated", names(coef(did_surface_impoundment_nonhap)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_surface_impoundment_nonhap, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "../latex/fig_did_total_onsite_releases_int_haps.pdf", width = 20, height = 14)
par(mfrow = c(4, 4))
fixest::iplot(did_total_releases_hap, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_total_releases_nonhap, xlim = c(2011, 2017), ylim = c(-0.7, 0.7), col = "blue",
              main = "Total Onsite Releases Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_hap, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Air Emissions Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_nonhap, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Air Emissions Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air_hap, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air_nonhap, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air_hap, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air_nonhap, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_water_disc_hap, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Surface Water Discharge Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_water_disc_nonhap, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_hap, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Land Releases Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_nonhap, xlim = c(2011, 2017), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment_hap, xlim = c(2011, 2017), ylim = c(-0.08, 0.08), col = "blue",
              main = "Total Surface Impoundment Intensity, HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment_nonhap, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-HAP", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Does the MW Policy affect emissions intensity of the Persistent Bioaccumulative Toxic Chemicals?
#======================================================================================================================#
### Onsite: Total releases intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ treated:post:pbt.chems +
    treated:post +
    treated:pbt.chems +
    post:pbt.chems +
    treated +
    pbt.chems +
    post +
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
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

did_total_releases_pbt <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(treated * year * pbt.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_total_releases_pbt, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_pbt, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_pbt)[grep(pattern = "treated", names(coef(did_total_releases_pbt)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_total_releases_pbt, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_total_releases_nonpbt <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_total_releases_nonpbt, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_nonpbt, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Releases Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_nonpbt)[grep(pattern = "treated", names(coef(did_total_releases_nonpbt)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_total_releases_nonpbt, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total air emissions intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ treated:post:pbt.chems +
    treated:post +
    treated:pbt.chems +
    post:pbt.chems +
    treated +
    pbt.chems +
    post +
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
fixest::etable(did_air, digits = 4, digits.stats = 4)

did_air_pbt <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year * pbt.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_air_pbt, digits = 4, digits.stats = 4)
fixest::iplot(did_air_pbt, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Air Emissions Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_pbt)[grep(pattern = "treated", names(coef(did_air_pbt)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_pbt, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_air_nonpbt <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_air_nonpbt, digits = 4, digits.stats = 4)
fixest::iplot(did_air_nonpbt, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Air Emissions Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_nonpbt)[grep(pattern = "treated", names(coef(did_air_nonpbt)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_nonpbt, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total point air emissions intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
did_point_air <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ treated:post:pbt.chems +
    treated:post +
    treated:pbt.chems +
    post:pbt.chems +
    treated +
    pbt.chems +
    post +
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

did_point_air_pbt <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(treated * year * pbt.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_point_air_pbt, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air_pbt, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air_pbt)[grep(pattern = "treated", names(coef(did_point_air_pbt)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_point_air_pbt, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_point_air_nonpbt <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_point_air_nonpbt, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air_nonpbt, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air_nonpbt)[grep(pattern = "treated", names(coef(did_point_air_nonpbt)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_point_air_nonpbt, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
did_fug_air <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ treated:post:pbt.chems +
    treated:post +
    treated:pbt.chems +
    post:pbt.chems +
    treated +
    pbt.chems +
    post +
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

did_fug_air_pbt <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(treated * year * pbt.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_fug_air_pbt, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air_pbt, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air_pbt)[grep(pattern = "treated", names(coef(did_fug_air_pbt)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_fug_air_pbt, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_fug_air_nonpbt <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_fug_air_nonpbt, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air_nonpbt, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air_nonpbt)[grep(pattern = "treated", names(coef(did_fug_air_nonpbt)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_fug_air_nonpbt, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ treated:post:pbt.chems +
    treated:post +
    treated:pbt.chems +
    post:pbt.chems +
    treated +
    pbt.chems +
    post +
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

did_water_disc_pbt <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year * pbt.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_water_disc_pbt, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc_pbt, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Surface Water Discharge Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc_pbt)[grep(pattern = "treated", names(coef(did_water_disc_pbt)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_water_disc_pbt, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_water_disc_nonpbt <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_water_disc_nonpbt, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc_nonpbt, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc_nonpbt)[grep(pattern = "treated", names(coef(did_water_disc_nonpbt)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_water_disc_nonpbt, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total land releases intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ treated:post:pbt.chems +
    treated:post +
    treated:pbt.chems +
    post:pbt.chems +
    treated +
    pbt.chems +
    post +
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

did_land_releases_pbt <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year * pbt.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_land_releases_pbt, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_pbt, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Land Releases Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_pbt)[grep(pattern = "treated", names(coef(did_land_releases_pbt)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_pbt, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_land_releases_nonpbt <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_land_releases_nonpbt, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_nonpbt, xlim = c(2011, 2017), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_nonpbt)[grep(pattern = "treated", names(coef(did_land_releases_nonpbt)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_nonpbt, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
did_surface_impoundment <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ treated:post:pbt.chems +
    treated:post +
    treated:pbt.chems +
    post:pbt.chems +
    treated +
    pbt.chems +
    post +
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

did_surface_impoundment_pbt <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~
    i(treated * year * pbt.chems, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_surface_impoundment_pbt, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impoundment_pbt, xlim = c(2011, 2017), ylim = c(-0.08, 0.08), col = "blue",
              main = "Total Surface Impoundment Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impoundment_pbt)[grep(pattern = "treated", names(coef(did_surface_impoundment_pbt)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_surface_impoundment_pbt, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_surface_impoundment_nonpbt <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_surface_impoundment_nonpbt, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impoundment_nonpbt, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impoundment_nonpbt)[grep(pattern = "treated", names(coef(did_surface_impoundment_nonpbt)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_surface_impoundment_nonpbt, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_did_total_onsite_releases_int_pbts.pdf", width = 20, height = 15)
par(mfrow = c(4, 4))
fixest::iplot(did_total_releases_pbt, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_total_releases_nonpbt, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Releases Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_pbt, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Air Emissions Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_nonpbt, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Air Emissions Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air_pbt, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Point Air Emissions Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air_nonpbt, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air_pbt, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air_nonpbt, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_water_disc_pbt, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Surface Water Discharge Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_water_disc_nonpbt, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_pbt, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Land Releases Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_nonpbt, xlim = c(2011, 2017), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment_pbt, xlim = c(2011, 2017), ylim = c(-0.08, 0.08), col = "blue",
              main = "Total Surface Impoundment Intensity, PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment_nonpbt, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, Non-PBTs", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
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
did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ treated:post:labour.intensity.high +
    treated:post +
    treated:labour.intensity.high +
    post:labour.intensity.high +
    treated +
    labour.intensity.high +
    post +
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
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

did_total_releases_highlabint <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(treated * year * labour.intensity.high, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_total_releases_highlabint, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_highlabint, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Releases Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_highlabint)[grep(pattern = "treated", names(coef(did_total_releases_highlabint)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_total_releases_highlabint, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_total_releases_lowlabint <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_total_releases_lowlabint, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_lowlabint, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Releases Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_lowlabint)[grep(pattern = "treated", names(coef(did_total_releases_lowlabint)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_total_releases_lowlabint, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total air emissions intensity (labour intensive industries)
#======================================================================================================================#
did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ treated:post:labour.intensity.high +
    treated:post +
    treated:labour.intensity.high +
    post:labour.intensity.high +
    treated +
    labour.intensity.high +
    post +
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
fixest::etable(did_air, digits = 4, digits.stats = 4)

did_air_highlabint <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year * labour.intensity.high, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_air_highlabint, digits = 4, digits.stats = 4)
fixest::iplot(did_air_highlabint, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Air Emissions Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_highlabint)[grep(pattern = "treated", names(coef(did_air_highlabint)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_highlabint, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_air_lowlabint <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_air_lowlabint, digits = 4, digits.stats = 4)
fixest::iplot(did_air_lowlabint, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Air Emissions Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_lowlabint)[grep(pattern = "treated", names(coef(did_air_lowlabint)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_lowlabint, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total point air emissions intensity (labour intensive industries)
#======================================================================================================================#
did_point_air <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ treated:post:labour.intensity.high +
    treated:post +
    treated:labour.intensity.high +
    post:labour.intensity.high +
    treated +
    labour.intensity.high +
    post +
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

did_point_air_highlabint <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(treated * year * labour.intensity.high, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_point_air_highlabint, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air_highlabint, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air_highlabint)[grep(pattern = "treated", names(coef(did_point_air_highlabint)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_point_air_highlabint, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_point_air_lowlabint <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_point_air_lowlabint, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air_lowlabint, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air_lowlabint)[grep(pattern = "treated", names(coef(did_point_air_lowlabint)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_point_air_lowlabint, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (labour intensive industries)
#======================================================================================================================#
did_fug_air <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ treated:post:labour.intensity.high +
    treated:post +
    treated:labour.intensity.high +
    post:labour.intensity.high +
    treated +
    labour.intensity.high +
    post +
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

did_fug_air_highlabint <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(treated * year * labour.intensity.high, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_fug_air_highlabint, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air_highlabint, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air_highlabint)[grep(pattern = "treated", names(coef(did_fug_air_highlabint)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_fug_air_highlabint, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_fug_air_lowlabint <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_fug_air_lowlabint, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air_lowlabint, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air_lowlabint)[grep(pattern = "treated", names(coef(did_fug_air_lowlabint)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_fug_air_lowlabint, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (labour intensive industries)
#======================================================================================================================#
did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ treated:post:labour.intensity.high +
    treated:post +
    treated:labour.intensity.high +
    post:labour.intensity.high +
    treated +
    labour.intensity.high +
    post +
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

did_water_disc_highlabint <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year * labour.intensity.high, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_water_disc_highlabint, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc_highlabint, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Surface Water Discharge Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc_highlabint)[grep(pattern = "treated", names(coef(did_water_disc_highlabint)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_water_disc_highlabint, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_water_disc_lowlabint <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_water_disc_lowlabint, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc_lowlabint, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc_lowlabint)[grep(pattern = "treated", names(coef(did_water_disc_lowlabint)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_water_disc_lowlabint, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total land releases intensity (labour intensive industries)
#======================================================================================================================#
did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ treated:post:labour.intensity.high +
    treated:post +
    treated:labour.intensity.high +
    post:labour.intensity.high +
    treated +
    labour.intensity.high +
    post +
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

did_land_releases_highlabint <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year * labour.intensity.high, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_land_releases_highlabint, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_highlabint, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Land Releases Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_highlabint)[grep(pattern = "treated", names(coef(did_land_releases_highlabint)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_highlabint, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_land_releases_lowlabint <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_land_releases_lowlabint, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_lowlabint, xlim = c(2011, 2017), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_lowlabint)[grep(pattern = "treated", names(coef(did_land_releases_lowlabint)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_lowlabint, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (labour intensive industries)
#======================================================================================================================#
did_surface_impoundment <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ treated:post:labour.intensity.high +
    treated:post +
    treated:labour.intensity.high +
    post:labour.intensity.high +
    treated +
    labour.intensity.high +
    post +
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

did_surface_impoundment_highlabint <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~
    i(treated * year * labour.intensity.high, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_surface_impoundment_highlabint, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impoundment_highlabint, xlim = c(2011, 2017), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Surface Impoundment Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impoundment_highlabint)[grep(pattern = "treated", names(coef(did_surface_impoundment_highlabint)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_surface_impoundment_highlabint, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_surface_impoundment_lowlabint <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_surface_impoundment_lowlabint, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impoundment_lowlabint, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impoundment_lowlabint)[grep(pattern = "treated", names(coef(did_surface_impoundment_lowlabint)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_surface_impoundment_lowlabint, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_did_total_onsite_releases_int_highlabint.pdf", width = 20, height = 15)
par(mfrow = c(4, 4))
fixest::iplot(did_total_releases_highlabint, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Releases Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_total_releases_lowlabint, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Releases Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_highlabint, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Air Emissions Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_lowlabint, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Air Emissions Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air_highlabint, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air_lowlabint, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air_highlabint, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air_lowlabint, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_water_disc_highlabint, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Surface Water Discharge Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_water_disc_lowlabint, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_highlabint, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Land Releases Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_lowlabint, xlim = c(2011, 2017), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment_highlabint, xlim = c(2011, 2017), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Surface Impoundment Intensity, HLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment_lowlabint, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, LLII", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
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
did_total_releases <- fixest::feols(
  l.total.releases.onsite.intensity ~ treated:post:low.ind.conc +
    treated:post +
    treated:low.ind.conc +
    post:low.ind.conc +
    treated +
    low.ind.conc +
    post +
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
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

did_total_releases_lowindconc <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(treated * year * low.ind.conc, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_total_releases_lowindconc, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_lowindconc, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Releases Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_lowindconc)[grep(pattern = "treated", names(coef(did_total_releases_lowindconc)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_total_releases_lowindconc, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_total_releases_highindconc <- fixest::feols(
  l.total.releases.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_total_releases_highindconc, digits = 4, digits.stats = 4)
fixest::iplot(did_total_releases_highindconc, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Releases Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases_highindconc)[grep(pattern = "treated", names(coef(did_total_releases_highindconc)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_total_releases_highindconc, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total air emissions intensity (industry concentration)
#======================================================================================================================#
did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ treated:post:low.ind.conc +
    treated:post +
    treated:low.ind.conc +
    post:low.ind.conc +
    treated +
    low.ind.conc +
    post +
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
fixest::etable(did_air, digits = 4, digits.stats = 4)

did_air_lowindconc <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year * low.ind.conc, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_air_lowindconc, digits = 4, digits.stats = 4)
fixest::iplot(did_air_lowindconc, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Air Emissions Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_lowindconc)[grep(pattern = "treated", names(coef(did_air_lowindconc)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_lowindconc, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_air_highindconc <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_air_highindconc, digits = 4, digits.stats = 4)
fixest::iplot(did_air_highindconc, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Air Emissions Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_highindconc)[grep(pattern = "treated", names(coef(did_air_highindconc)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_highindconc, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total point air emissions intensity (industry concentration)
#======================================================================================================================#
did_point_air <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ treated:post:low.ind.conc +
    treated:post +
    treated:low.ind.conc +
    post:low.ind.conc +
    treated +
    low.ind.conc +
    post +
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

did_point_air_lowindconc <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(treated * year * low.ind.conc, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_point_air_lowindconc, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air_lowindconc, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air_lowindconc)[grep(pattern = "treated", names(coef(did_point_air_lowindconc)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_point_air_lowindconc, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_point_air_highindconc <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_point_air_highindconc, digits = 4, digits.stats = 4)
fixest::iplot(did_point_air_highindconc, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air_highindconc)[grep(pattern = "treated", names(coef(did_point_air_highindconc)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_point_air_highindconc, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (industry concentration)
#======================================================================================================================#
did_fug_air <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ treated:post:low.ind.conc +
    treated:post +
    treated:low.ind.conc +
    post:low.ind.conc +
    treated +
    low.ind.conc +
    post +
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

did_fug_air_lowindconc <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(treated * year * low.ind.conc, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_fug_air_lowindconc, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air_lowindconc, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air_lowindconc)[grep(pattern = "treated", names(coef(did_fug_air_lowindconc)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_fug_air_lowindconc, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_fug_air_highindconc <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_fug_air_highindconc, digits = 4, digits.stats = 4)
fixest::iplot(did_fug_air_highindconc, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air_highindconc)[grep(pattern = "treated", names(coef(did_fug_air_highindconc)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_fug_air_highindconc, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (industry concentration)
#======================================================================================================================#
did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ treated:post:low.ind.conc +
    treated:post +
    treated:low.ind.conc +
    post:low.ind.conc +
    treated +
    low.ind.conc +
    post +
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

did_water_disc_lowindconc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year * low.ind.conc, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_water_disc_lowindconc, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc_lowindconc, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Surface Water Discharge Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc_lowindconc)[grep(pattern = "treated", names(coef(did_water_disc_lowindconc)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_water_disc_lowindconc, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_water_disc_highindconc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_water_disc_highindconc, digits = 4, digits.stats = 4)
fixest::iplot(did_water_disc_highindconc, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc_highindconc)[grep(pattern = "treated", names(coef(did_water_disc_highindconc)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_water_disc_highindconc, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total land releases intensity (industry concentration)
#======================================================================================================================#
did_land_releases <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ treated:post:low.ind.conc +
    treated:post +
    treated:low.ind.conc +
    post:low.ind.conc +
    treated +
    low.ind.conc +
    post +
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

did_land_releases_lowindconc <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year * low.ind.conc, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_land_releases_lowindconc, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_lowindconc, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Land Releases Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_lowindconc)[grep(pattern = "treated", names(coef(did_land_releases_lowindconc)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_lowindconc, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_land_releases_highindconc <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_land_releases_highindconc, digits = 4, digits.stats = 4)
fixest::iplot(did_land_releases_highindconc, xlim = c(2011, 2017), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_highindconc)[grep(pattern = "treated", names(coef(did_land_releases_highindconc)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_land_releases_highindconc, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (industry concentration)
#======================================================================================================================#
did_surface_impoundment <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ treated:post:low.ind.conc +
    treated:post +
    treated:low.ind.conc +
    post:low.ind.conc +
    treated +
    low.ind.conc +
    post +
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

did_surface_impoundment_lowindconc <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~
    i(treated * year * low.ind.conc, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_surface_impoundment_lowindconc, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impoundment_lowindconc, xlim = c(2011, 2017), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Surface Impoundment Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impoundment_lowindconc)[grep(pattern = "treated", names(coef(did_surface_impoundment_lowindconc)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_surface_impoundment_lowindconc, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_surface_impoundment_highindconc <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~
    i(treated * year, ref = c(2013, 0)) +
      treated:post +
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
fixest::etable(did_surface_impoundment_highindconc, digits = 4, digits.stats = 4)
fixest::iplot(did_surface_impoundment_highindconc, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impoundment_highindconc)[grep(pattern = "treated", names(coef(did_surface_impoundment_highindconc)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_surface_impoundment_highindconc, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_did_total_onsite_releases_int_lowindconc.pdf", width = 20, height = 15)
par(mfrow = c(4, 4))
fixest::iplot(did_total_releases_lowindconc, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Releases Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_total_releases_highindconc, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Releases Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_lowindconc, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Air Emissions Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_highindconc, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Air Emissions Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air_lowindconc, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_point_air_highindconc, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Total Point Air Emissions Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air_lowindconc, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_fug_air_highindconc, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Fugitive Air Emissions Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_water_disc_lowindconc, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Surface Water Discharge Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_water_disc_highindconc, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Surface Water Discharge Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_lowindconc, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Total Land Releases Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_land_releases_highindconc, xlim = c(2011, 2017), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Land Releases Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment_lowindconc, xlim = c(2011, 2017), ylim = c(-0.15, 0.15), col = "blue",
              main = "Total Surface Impoundment Intensity, LCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_surface_impoundment_highindconc, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Total Surface Impoundment Intensity, HCI", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#