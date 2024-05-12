#======================================================================================================================#
### Loading Data
#======================================================================================================================#
file <- "./Data_PhD/US/BLS/onsite/triQc_on.rds"
triQc <- read_rds(file = file)
#======================================================================================================================#
### Triple Difference: Onsite Releases Intensity
### Economic Growth Patterns: GDP
### Higher GDP counties are above the median GDP, and low GDP counties are below the median GDP.
#======================================================================================================================#
# median_gdp <- median(triQc$gdp)
# triQc$high.gdp <- ifelse(triQc$gdp > median_gdp, yes = 1, no = 0)
# triQc$low.gdp <- ifelse(triQc$gdp < median_gdp, yes = 1, no = 0)

gdp_50th_percentile <- quantile(triQc$gdp, probs = 0.5) # same as above
triQc$high.gdp <- ifelse(triQc$gdp > gdp_50th_percentile, yes = 1, no = 0)
triQc$low.gdp <- ifelse(triQc$gdp < gdp_50th_percentile, yes = 1, no = 0)

prop.table(table(triQc$high.gdp)) * 100
prop.table(table(triQc$low.gdp)) * 100
#======================================================================================================================#
### Onsite: Total releases intensity
#======================================================================================================================#
did_total_releases <- fixest::feols(
  # l.total.releases.onsite.intensity ~ e.treated * high.gdp +
  l.total.releases.onsite.intensity ~ treated:post:high.gdp +
    treated:post +
    high.gdp +
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
fixest::etable(did_total_releases, digits = 4, digits.stats = 4)

did_total_releases_hgdp <- fixest::feols(
  l.total.releases.onsite.intensity ~
    # i(rel.year, ref = c(2013, Inf)):high.gdp +
    i(treated * year * high.gdp, ref = c(2013, 0)) +
      # e.treated +
      treated:post +
      high.gdp +
      gdppc.1 +
      annual.avg.estabs.1 +
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
    # i(rel.year, ref = c(2013, Inf)):low.gdp +
    i(treated * year * low.gdp, ref = c(2013, 0)) +
      # e.treated +
      treated:post +
      low.gdp +
      gdppc.1 +
      annual.avg.estabs.1 +
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
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_did_total_releases_onsite_int_GDP.pdf", width = 12, height = 4.5)
par(mfrow = c(1, 2))
fixest::iplot(did_total_releases_hgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_total_releases_lgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Onsite: Total air emissions intensity
#======================================================================================================================#
did_air <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ treated:post:high.gdp +
    treated:post +
    high.gdp +
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

did_air_hgdp <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year * high.gdp, ref = c(2013, Inf)) +
      treated:post +
      high.gdp +
      gdppc.1 +
      annual.avg.estabs.1 +
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
fixest::etable(did_air_hgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_air_hgdp, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Air Emissions Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_hgdp)[grep(pattern = "treated", names(coef(did_air_hgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_hgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")

did_air_lgdp <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    i(treated * year * low.gdp, ref = c(2013, Inf)) +
      treated:post +
      low.gdp +
      gdppc.1 +
      annual.avg.estabs.1 +
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
fixest::etable(did_air_lgdp, digits = 4, digits.stats = 4)
fixest::iplot(did_air_lgdp, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Air Emissions Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air_lgdp)[grep(pattern = "treated", names(coef(did_air_lgdp)))]
pre.treat.coef <- pre.treat.coef[1:2]
linearHypothesis(did_air_lgdp, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Onsite: Total surface water discharge intensity
#======================================================================================================================#
did_water_disc <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ treated:post:high.gdp +
    treated:post +
    high.gdp +
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

did_water_disc_hgdp <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~
    i(treated * year * high.gdp, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
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
    i(treated * year * low.gdp, ref = c(2013, Inf)) +
      treated:post +
      low.gdp +
      gdppc.1 +
      annual.avg.estabs.1 +
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
  l.total.land.releases.onsite.intensity ~ treated:post:gdp +
    treated:post +
    high.gdp +
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

did_land_releases_hgdp <- fixest::feols(
  l.total.land.releases.onsite.intensity ~
    i(treated * year * high.gdp, ref = c(2013, Inf)) +
      treated:post +
      high.gdp +
      gdppc.1 +
      annual.avg.estabs.1 +
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
    i(treated * year * low.gdp, ref = c(2013, Inf)) +
      treated:post +
      low.gdp +
      gdppc.1 +
      annual.avg.estabs.1 +
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
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_did_total_releases_onsite_int_GDP.pdf", width = 20, height = 8)
par(mfrow = c(2, 4))
fixest::iplot(did_total_releases_hgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_total_releases_lgdp, xlim = c(2011, 2017), ylim = c(-0.9, 0.9), col = "blue",
              main = "Total Onsite Releases Intensity, low GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
fixest::iplot(did_air_hgdp, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Air Emissions Intensity, high GDP counties", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)

fixest::iplot(did_air_lgdp, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Total Onsite Air Emissions Intensity, low GDP counties", xlab = "relative year",
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
dev.off()
#======================================================================================================================#