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
### Triple Differences
#======================================================================================================================#
### Financial Constraints
### I proxy financial constraints by the ratio of industries profit margin. That is, the ratio of industry revenue to
# profit, which measures the proportion of industry revenue allocated to profit. Industries with profit margin lower
# than the median profit margin are more financially constrained, and those above are less.
#======================================================================================================================#
triQc <- triQc %>%
  group_by(year, naics.code) %>%
  rename(revenue = vship) %>%
  mutate(
	total.cost = matcost + prodw + energy,
	profit = revenue - total.cost,
	revenue.to.profit = (revenue / profit) * 100,
	# Operational efficiency ratio
	payroll.to.revenue = pay / revenue,
	wages.to.revenue = prodw / revenue,
	# Cost efficiency ratio
	energy.to.revenue = energy / revenue,
  ) %>%
  ungroup() %>%
  mutate(
	high.profit.margin = case_when(revenue.to.profit > mean(revenue.to.profit) ~ 1, TRUE ~ 0),
	high.payroll.revenue = case_when(payroll.to.revenue > mean(payroll.to.revenue) ~ 1, TRUE ~ 0),
	high.wages.revenue = case_when(wages.to.revenue > mean(wages.to.revenue) ~ 1, TRUE ~ 0),
	high.energy.revenue = case_when(energy.to.revenue > mean(energy.to.revenue) ~ 1, TRUE ~ 0),
  )

triQc %>% sum_up(
  c(high.payroll.revenue, high.wages.revenue, high.energy.revenue, high.profit.margin)
)
#======================================================================================================================#
### Onsite: High Profit Margin ratio (financial constraints)
#======================================================================================================================#
### Onsite: Total releases intensity (high revenue to profit ratio)
#======================================================================================================================#
sdid_total_releases_hrpr <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_hrpr, digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_hrpr, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
			  main = "Total Onsite Releases Intensity, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_lrpr <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.profit.margin +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_lrpr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_lrpr, xlim = c(-3, 3), ylim = c(-0.3, 0.4), col = "blue",
			  main = "Total Onsite Releases Intensity, LRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (high revenue to profit ratio)
#======================================================================================================================#
sdid_air_hrpr <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_hrpr, digits = 3, digits.stats = 3)
fixest::etable(sdid_air_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_hrpr, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_lrpr <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.profit.margin +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_lrpr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_lrpr, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, LRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (revenue to profit ratio)
#======================================================================================================================#
sdid_point_air_hrpr <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_hrpr, digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_hrpr, xlim = c(-3, 3), ylim = c(-0.3, 0.5), col = "blue",
			  main = "Total Point Air Emissions Intensity, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_lrpr <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.profit.margin +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_lrpr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_lrpr, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
			  main = "Total Point Air Emissions Intensity, LRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Fugitive air emissions intensity (revenue to profit ratio)
#======================================================================================================================#
sdid_fug_air_hrpr <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_hrpr, digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_hrpr, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_lrpr <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.profit.margin +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_lrpr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_lrpr, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, LRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Water Discharge intensity (revenue to profit ratio)
#======================================================================================================================#
sdid_water_disc_hrpr <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_hrpr, digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_hrpr, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
			  main = "Total Surface Water Discharge Intensity, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_lrpr <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.profit.margin +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_lrpr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_lrpr, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
			  main = "Total Surface Water Discharge Intensity, LRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Land Releases intensity (revenue to profit ratio)
#======================================================================================================================#
sdid_land_releases_hrpr <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_hrpr, digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_hrpr, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
			  main = "Total Land Releases Intensity, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_lrpr <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.profit.margin +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_lrpr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_lrpr, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
			  main = "Total Land Releases Intensity, LRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Impoundment intensity (revenue to profit ratio)
#======================================================================================================================#
sdid_surface_impoundment_hrpr <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_hrpr, digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_hrpr, xlim = c(-3, 3), ylim = c(-0.01, 0.03), col = "blue",
			  main = "Total Surface Impoundment Intensity, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_lrpr <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.profit.margin +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_lrpr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_lrpr, xlim = c(-3, 3), ylim = c(-0.01, 0.04), col = "blue",
			  main = "Total Surface Impoundment Intensity, LRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_finc.pdf", width = 22,
	height = 10)
par(mfrow = c(3, 5))
fixest::iplot(sdid_total_releases_hrpr, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
			  main = "Total Onsite Releases Intensity, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_total_releases_lrpr, xlim = c(-3, 3), ylim = c(-0.3, 0.4), col = "blue",
			  main = "Total Onsite Releases Intensity, LRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_hrpr, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_lrpr, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, LRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_hrpr, xlim = c(-3, 3), ylim = c(-0.3, 0.5), col = "blue",
			  main = "Total Point Air Emissions Intensity, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_lrpr, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
			  main = "Total Point Air Emissions Intensity, LRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_hrpr, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_lrpr, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, LRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_hrpr, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
			  main = "Total Surface Water Discharge Intensity, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_lrpr, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
			  main = "Total Surface Water Discharge Intensity, LRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_hrpr, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
			  main = "Total Land Releases Intensity, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_lrpr, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
			  main = "Total Land Releases Intensity, LRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_hrpr, xlim = c(-3, 3), ylim = c(-0.01, 0.03), col = "blue",
			  main = "Total Surface Impoundment Intensity, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_lrpr, xlim = c(-3, 3), ylim = c(-0.01, 0.04), col = "blue",
			  main = "Total Surface Impoundment Intensity, LRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Type of Production Technology
#======================================================================================================================#
### Onsite: Total releases intensity (high payroll to revenue ratio)---Poor operational efficiency
#======================================================================================================================#
sdid_total_releases_hprr <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_hprr, digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_hprr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_hprr, xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = "blue",
			  main = "Total Onsite Releases Intensity, HPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_lprr <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.payroll.revenue +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_lprr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_lprr, xlim = c(-3, 3), ylim = c(-0.2, 0.5), col = "blue",
			  main = "Total Onsite Releases Intensity, LPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (high payroll to revenue ratio)---Poor operational efficiency
#======================================================================================================================#
sdid_air_hprr <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_hprr, digits = 3, digits.stats = 3)
fixest::etable(sdid_air_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_hprr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_hprr, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, HPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_lprr <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.payroll.revenue +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_lprr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_lprr, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, LPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (payroll to revenue ratio)
#======================================================================================================================#
sdid_point_air_hprr <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_hprr, digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_hprr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_hprr, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
			  main = "Total Point Air Emissions Intensity, HPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_lprr <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.payroll.revenue +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_lprr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_lprr, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
			  main = "Total Point Air Emissions Intensity, LPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Fugitive air emissions intensity (payroll to revenue ratio)
#======================================================================================================================#
sdid_fug_air_hprr <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_hprr, digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_hprr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_hprr, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, HPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_lprr <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.payroll.revenue +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_lprr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_lprr, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, LPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Water Discharge intensity (payroll to revenue ratio)
#======================================================================================================================#
sdid_water_disc_hprr <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_hprr, digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_hprr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_hprr, xlim = c(-3, 3), ylim = c(-0.2, 0.1), col = "blue",
			  main = "Total Surface Water Discharge Intensity, HPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_lprr <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.payroll.revenue +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_lprr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_lprr, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
			  main = "Total Surface Water Discharge Intensity, LPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Land Releases intensity (payroll to revenue ratio)
#======================================================================================================================#
sdid_land_releases_hprr <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_hprr, digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_hprr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_hprr, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
			  main = "Total Land Releases Intensity, HPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_lprr <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.payroll.revenue +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_lprr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_lprr, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
			  main = "Total Land Releases Intensity, LPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Impoundment intensity (payroll to revenue ratio)
#======================================================================================================================#
sdid_surface_impoundment_hprr <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_hprr, digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_hprr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_hprr, xlim = c(-3, 3), ylim = c(-0.05, 0.05), col = "blue",
			  main = "Total Surface Impoundment Intensity, HPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_lprr <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.payroll.revenue +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_lprr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_lprr, xlim = c(-3, 3), ylim = c(-0.01, 0.07), col = "blue",
			  main = "Total Surface Impoundment Intensity, LPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_tech_payroll.pdf", width = 22,
	height = 10)
par(mfrow = c(3, 5))
fixest::iplot(sdid_total_releases_hprr, xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = "blue",
			  main = "Total Onsite Releases Intensity, HPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_total_releases_lprr, xlim = c(-3, 3), ylim = c(-0.2, 0.5), col = "blue",
			  main = "Total Onsite Releases Intensity, LPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_hprr, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, HPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_lprr, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, LPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_hprr, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
			  main = "Total Point Air Emissions Intensity, HPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_lprr, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
			  main = "Total Point Air Emissions Intensity, LPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_hprr, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, HPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_lprr, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, LPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_hprr, xlim = c(-3, 3), ylim = c(-0.2, 0.1), col = "blue",
			  main = "Total Surface Water Discharge Intensity, HPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_lprr, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
			  main = "Total Surface Water Discharge Intensity, LPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_hprr, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
			  main = "Total Land Releases Intensity, HPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_lprr, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
			  main = "Total Land Releases Intensity, LPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_hprr, xlim = c(-3, 3), ylim = c(-0.05, 0.05), col = "blue",
			  main = "Total Surface Impoundment Intensity, HPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_lprr, xlim = c(-3, 3), ylim = c(-0.01, 0.07), col = "blue",
			  main = "Total Surface Impoundment Intensity, LPRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Onsite: Total releases intensity (high wages to revenue ratio)---Poor operational efficiency
#======================================================================================================================#
sdid_total_releases_hwrr <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_hwrr, digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_hwrr, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = "blue",
			  main = "Total Onsite Releases Intensity, HWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_lwrr <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.wages.revenue +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_lwrr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_lwrr, xlim = c(-3, 3), ylim = c(-0.3, 0.5), col = "blue",
			  main = "Total Onsite Releases Intensity, LWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (high wages to revenue ratio)---Poor operational efficiency
#======================================================================================================================#
sdid_air_hwrr <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_hwrr, digits = 3, digits.stats = 3)
fixest::etable(sdid_air_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_hwrr, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, HWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_lwrr <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.wages.revenue +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_lwrr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_lwrr, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, LWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (wages to revenue ratio)
#======================================================================================================================#
sdid_point_air_hwrr <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_hwrr, digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_hwrr, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
			  main = "Total Point Air Emissions Intensity, HWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_lwrr <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.wages.revenue +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_lwrr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_lwrr, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
			  main = "Total Point Air Emissions Intensity, LWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Fugitive air emissions intensity (wages to revenue ratio)
#======================================================================================================================#
sdid_fug_air_hwrr <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_hwrr, digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_hwrr, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, HWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_lwrr <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.wages.revenue +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_lwrr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_lwrr, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, LWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Water Discharge intensity (wages to revenue ratio)
#======================================================================================================================#
sdid_water_disc_hwrr <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.energy.revenue +
	treated +
	high.wages.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_hwrr, digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_hwrr, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
			  main = "Total Surface Water Discharge Intensity, HWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_lwrr <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.wages.revenue +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_lwrr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_lwrr, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
			  main = "Total Surface Water Discharge Intensity, LWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Land Releases intensity (wages to revenue ratio)
#======================================================================================================================#
sdid_land_releases_hwrr <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_hwrr, digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_hwrr, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
			  main = "Total Land Releases Intensity, HWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_lwrr <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.wages.revenue +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_lwrr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_lwrr, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
			  main = "Total Land Releases Intensity, LWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Impoundment intensity (wages to revenue ratio)
#======================================================================================================================#
sdid_surface_impoundment_hwrr <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_hwrr, digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_hwrr, xlim = c(-3, 3), ylim = c(-0.02, 0.04), col = "blue",
			  main = "Total Surface Impoundment Intensity, HWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_lwrr <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:high.wages.revenue +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_lwrr, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_lwrr, xlim = c(-3, 3), ylim = c(-0.01, 0.04), col = "blue",
			  main = "Total Surface Impoundment Intensity, LWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_tech_wages.pdf", width = 22,
	height = 10)
par(mfrow = c(3, 5))
fixest::iplot(sdid_total_releases_hwrr, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = "blue",
			  main = "Total Onsite Releases Intensity, HWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_total_releases_lwrr, xlim = c(-3, 3), ylim = c(-0.3, 0.5), col = "blue",
			  main = "Total Onsite Releases Intensity, LWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_hwrr, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, HWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_lwrr, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, LWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_hwrr, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
			  main = "Total Point Air Emissions Intensity, HWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_lwrr, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
			  main = "Total Point Air Emissions Intensity, LWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_hwrr, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, HWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_lwrr, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, LWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_hwrr, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
			  main = "Total Surface Water Discharge Intensity, HWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_lwrr, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
			  main = "Total Surface Water Discharge Intensity, LWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_hwrr, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
			  main = "Total Land Releases Intensity, HWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_lwrr, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
			  main = "Total Land Releases Intensity, LWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_hwrr, xlim = c(-3, 3), ylim = c(-0.02, 0.04), col = "blue",
			  main = "Total Surface Impoundment Intensity, HWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_lwrr, xlim = c(-3, 3), ylim = c(-0.01, 0.04), col = "blue",
			  main = "Total Surface Impoundment Intensity, LWRR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Highest Emitting Industries
#======================================================================================================================#
triQc <- triQc %>%
  mutate(
	highest.emitt.ind = case_when(
	  industry.name %in% c("Chemical Manufacturing", "Food Manufacturing",
						   "Leather and Allied Product Manufacturing", "Wood Product Manufacturing") ~ 1, T ~ 0
	)
  )
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_surface_impound_lowemitt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impound_lowemitt, xlim = c(-3, 3), ylim = c(-0.01, 0.05), col = "blue",
			  main = "Total Surface Impoundment Intensity, LEIs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_EMITT.pdf", width = 22,
	height = 10)
par(mfrow = c(3, 5))
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
fixest::iplot(sdid_surface_impound_highemitt, xlim = c(-3, 3), ylim = c(-0.03, 0.02), col = "blue",
			  main = "Total Surface Impoundment Intensity, HEIs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impound_lowemitt, xlim = c(-3, 3), ylim = c(-0.01, 0.05), col = "blue",
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
### Onsite: Total releases intensity
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impound_lgdp, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impound_lgdp, xlim = c(-3, 3), ylim = c(-0.02, 0.04), col = "blue",
			  main = "Total Surface Impoundment Intensity, low GDP counties", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_GDP.pdf", width = 22,
	height = 10)
par(mfrow = c(3, 5))
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
fixest::iplot(sdid_land_releases_lgdp, xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = "blue",
			  main = "Total Onsite Land Releases Intensity, low GDP counties", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impound_hgdp, xlim = c(-3, 3), ylim = c(-0.02, 0.02), col = "blue",
			  main = "Total Surface Impoundment Intensity, high GDP counties", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impound_lgdp, xlim = c(-3, 3), ylim = c(-0.02, 0.04), col = "blue",
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_carcinogenic, digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_carcinogenic, xlim = c(-3, 3), ylim = c(-0.6, 0.5), col = "blue",
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_noncarcinogenic, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.02, 0.05), col = "blue",
			  main = "Total Surface Impoundment Intensity, Non-Carcinogens", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_carcinogens.pdf", width =
  20, height = 10)
par(mfrow = c(3, 5))
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
fixest::iplot(sdid_surface_impoundment_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.02, 0.05), col = "blue",
			  main = "Total Surface Impoundment Intensity, Non-Carcinogens", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Does the MW Policy affect emissions intensity of the clean air act or haps regulated chemicals?
#======================================================================================================================#
triQc <- triQc %>% mutate(caa.haps = ifelse(test = clean.air.act.chems == 1 | hap.chems == 1, yes = 1, no = 0))
#======================================================================================================================#
### Onsite: Total releases intensity (clean air act or haps regulated chemicals)
#======================================================================================================================#
sdid_total_releases_caahaps <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year):caa.haps +
	e.treated +
	treated:caa.haps +
	post:caa.haps +
	treated +
	caa.haps +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_caahaps, digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_caahaps, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_total_releases_caahaps, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_caahaps, xlim = c(-3, 3), ylim = c(-1.3, 0.5), col = "blue",
			  main = "Total Onsite Releases Intensity, CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_noncaahaps <- fixest::feols(
  l.total.releases.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:caa.haps +
	treated:caa.haps +
	post:caa.haps +
	treated +
	caa.haps +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_total_releases_noncaahaps, digits = 3, digits.stats = 3)
fixest::iplot(sdid_total_releases_noncaahaps, xlim = c(-3, 3), ylim = c(-0.5, 1.5), col = "blue",
			  main = "Total Onsite Releases Intensity, Non-CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (clean air act or haps regulated chemicals)
#======================================================================================================================#
sdid_air_caahaps <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year):caa.haps +
	e.treated +
	treated:caa.haps +
	post:caa.haps +
	treated +
	caa.haps +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_caahaps, digits = 3, digits.stats = 3)
fixest::etable(sdid_air_caahaps, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_caahaps, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_caahaps, xlim = c(-3, 3), ylim = c(-1.1, 0.7), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_noncaahaps <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:caa.haps +
	treated:caa.haps +
	post:caa.haps +
	treated +
	caa.haps +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_noncaahaps, digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_noncaahaps, xlim = c(-3, 3), ylim = c(-0.5, 1.3), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, Non-CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (clean air act or haps regulated chemicals)
#======================================================================================================================#
sdid_point_air_caahaps <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year):caa.haps +
	e.treated +
	treated:caa.haps +
	post:caa.haps +
	treated +
	caa.haps +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_caahaps, digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_caahaps, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_point_air_caahaps, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_caahaps, xlim = c(-3, 3), ylim = c(-0.8, 0.5), col = "blue",
			  main = "Total Point Air Emissions Intensity, CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_noncaahaps <- fixest::feols(
  l.total.point.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:caa.haps +
	treated:caa.haps +
	post:caa.haps +
	treated +
	caa.haps +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_point_air_noncaahaps, digits = 3, digits.stats = 3)
fixest::iplot(sdid_point_air_noncaahaps, xlim = c(-3, 3), ylim = c(-0.3, 0.9), col = "blue",
			  main = "Total Point Air Emissions Intensity, Non-CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (clean air act or haps regulated chemicals)
#======================================================================================================================#
sdid_fug_air_caahaps <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year):caa.haps +
	e.treated +
	treated:caa.haps +
	post:caa.haps +
	treated +
	caa.haps +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_caahaps, digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_caahaps, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_caahaps, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_caahaps, xlim = c(-3, 3), ylim = c(-1.7, 0.5), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_noncaahaps <- fixest::feols(
  l.total.fug.air.emissions.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:caa.haps +
	treated:caa.haps +
	post:caa.haps +
	treated +
	caa.haps +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_noncaahaps, digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_noncaahaps, xlim = c(-3, 3), ylim = c(-0.4, 1.8), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, Non-CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (clean air act or haps regulated chemicals)
#======================================================================================================================#
sdid_water_disc_caahaps <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year):caa.haps +
	e.treated +
	treated:caa.haps +
	post:caa.haps +
	treated +
	caa.haps +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_caahaps, digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_caahaps, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_water_disc_caahaps, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_caahaps, xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = "blue",
			  main = "Total Surface Water Discharge Intensity, CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_noncaahaps <- fixest::feols(
  l.total.surface.water.discharge.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:caa.haps +
	treated:caa.haps +
	post:caa.haps +
	treated +
	caa.haps +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_water_disc_noncaahaps, digits = 3, digits.stats = 3)
fixest::iplot(sdid_water_disc_noncaahaps, xlim = c(-3, 3), ylim = c(-0.2, 0.5), col = "blue",
			  main = "Total Surface Water Discharge Intensity, Non-CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity (clean air act or haps regulated chemicals)
#======================================================================================================================#
sdid_land_releases_caahaps <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year):caa.haps +
	e.treated +
	treated:caa.haps +
	post:caa.haps +
	treated +
	caa.haps +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_caahaps, digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_caahaps, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_land_releases_caahaps, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_caahaps, xlim = c(-3, 3), ylim = c(-0.2, 0.1), col = "blue",
			  main = "Total Land Releases Intensity, CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_noncaahaps <- fixest::feols(
  l.total.land.releases.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:caa.haps +
	treated:caa.haps +
	post:caa.haps +
	treated +
	caa.haps +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_noncaahaps, digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_noncaahaps, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
			  main = "Total Land Releases Intensity, Non-CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (clean air act or haps regulated chemicals)
#======================================================================================================================#
sdid_surface_impoundment_caahaps <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year):caa.haps +
	e.treated +
	treated:caa.haps +
	post:caa.haps +
	treated +
	caa.haps +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_caahaps, digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_caahaps, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_surface_impoundment_caahaps, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_caahaps, xlim = c(-3, 3), ylim = c(-0.02, 0.03), col = "blue",
			  main = "Total Surface Impoundment Intensity, CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_noncaahaps <- fixest::feols(
  l.total.surface.impoundment.onsite.intensity ~ sunab(ch.year, year) +
	e.treated:caa.haps +
	treated:caa.haps +
	post:caa.haps +
	treated +
	caa.haps +
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_noncaahaps, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_noncaahaps, xlim = c(-3, 3), ylim = c(-0.01, 0.04), col = "blue",
			  main = "Total Surface Impoundment Intensity, Non-CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_caahaps.pdf", width = 22,
	height = 10)
par(mfrow = c(3, 5))
fixest::iplot(sdid_total_releases_caahaps, xlim = c(-3, 3), ylim = c(-1.3, 0.5), col = "blue",
			  main = "Total Onsite Releases Intensity, CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_total_releases_noncaahaps, xlim = c(-3, 3), ylim = c(-0.5, 1.5), col = "blue",
			  main = "Total Onsite Releases Intensity, Non-CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_caahaps, xlim = c(-3, 3), ylim = c(-1.1, 0.7), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_noncaahaps, xlim = c(-3, 3), ylim = c(-0.5, 1.3), col = "blue",
			  main = "Total Onsite Air Emissions Intensity, Non-CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_caahaps, xlim = c(-3, 3), ylim = c(-0.8, 0.5), col = "blue",
			  main = "Total Point Air Emissions Intensity, CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_point_air_noncaahaps, xlim = c(-3, 3), ylim = c(-0.3, 0.9), col = "blue",
			  main = "Total Point Air Emissions Intensity, Non-CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_caahaps, xlim = c(-3, 3), ylim = c(-1.7, 0.5), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_fug_air_noncaahaps, xlim = c(-3, 3), ylim = c(-0.4, 1.8), col = "blue",
			  main = "Total Fugitive Air Emissions Intensity, Non-CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_caahaps, xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = "blue",
			  main = "Total Surface Water Discharge Intensity, CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_water_disc_noncaahaps, xlim = c(-3, 3), ylim = c(-0.2, 0.5), col = "blue",
			  main = "Total Surface Water Discharge Intensity, Non-CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_caahaps, xlim = c(-3, 3), ylim = c(-0.2, 0.1), col = "blue",
			  main = "Total Land Releases Intensity, CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_land_releases_noncaahaps, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
			  main = "Total Land Releases Intensity, Non-CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_caahaps, xlim = c(-3, 3), ylim = c(-0.02, 0.03), col = "blue",
			  main = "Total Surface Impoundment Intensity, CAAHAPs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_noncaahaps, xlim = c(-3, 3), ylim = c(-0.01, 0.04), col = "blue",
			  main = "Total Surface Impoundment Intensity, Non-CAAHAPs", xlab = "relative year",
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_pbt, digits = 3, digits.stats = 3)
fixest::etable(sdid_air_pbt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_pbt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_pbt, xlim = c(-3, 3), ylim = c(-0.8, 0.7), col = "blue",
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_air_nonpbt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_air_nonpbt, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_fug_air_pbt, digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_pbt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_fug_air_pbt, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_fug_air_pbt, xlim = c(-3, 3), ylim = c(-0.6, 0.5), col = "blue",
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_land_releases_nonpbt, digits = 3, digits.stats = 3)
fixest::iplot(sdid_land_releases_nonpbt, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_pbts.pdf", width = 22,
	height = 10)
par(mfrow = c(3, 5))
fixest::iplot(sdid_total_releases_pbt, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
			  main = "Total Onsite Releases Intensity, PBTs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_total_releases_nonpbt, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
			  main = "Total Onsite Releases Intensity, Non-PBTs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_pbt, xlim = c(-3, 3), ylim = c(-0.8, 0.7), col = "blue",
			  main = "Total Air Emissions Intensity, PBTs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_air_nonpbt, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
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
fixest::iplot(sdid_fug_air_pbt, xlim = c(-3, 3), ylim = c(-0.6, 0.5), col = "blue",
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
fixest::iplot(sdid_land_releases_nonpbt, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
			  main = "Total Land Releases Intensity, Non-PBTs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_pbt, xlim = c(-3, 3), ylim = c(-0.05, 0.05), col = "blue",
			  main = "Total Surface Impoundment Intensity, PBTs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(sdid_surface_impoundment_nonpbt, xlim = c(-3, 3), ylim = c(-0.02, 0.05), col = "blue",
			  main = "Total Surface Impoundment Intensity, Non-PBTs", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Heifindahl Hirschman Index (HHI)---Industry concentration
#======================================================================================================================#
triQc <- triQc %>%
  # rename(revenue = vship) %>%
  group_by(naics.code) %>%
  mutate(
	revenue.2 = revenue^2, # get the squared revenue by industry
	sum.revenue.2 = sum(revenue.2, na.rm = T), # get the sum of squared revenue by industry
  ) %>%
  group_by(year, naics.code) %>%
  mutate(
	hhi = mean(sum.revenue.2, na.rm = T), # get the mean of squared revenue by industry to get HHI
  ) %>%
  ungroup()

# low HHI, less industry concentration and more competition
triQc$low.ind.conc <- as.numeric(triQc$hhi < median(triQc$hhi))
sum_up(triQc, c(revenue, revenue.2, sum.revenue.2, hhi, low.ind.conc))
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
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
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state),
)
fixest::etable(sdid_surface_impoundment_highindconc, digits = 3, digits.stats = 3)
fixest::iplot(sdid_surface_impoundment_highindconc, xlim = c(-3, 3), ylim = c(-0.02, 0.06), col = "blue",
			  main = "Total Surface Impoundment Intensity, HCI", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_lowindconc.pdf", width =
  20, height = 10)
par(mfrow = c(3, 5))
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
fixest::iplot(sdid_surface_impoundment_highindconc, xlim = c(-3, 3), ylim = c(-0.02, 0.06), col = "blue",
			  main = "Total Surface Impoundment Intensity, HCI", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#