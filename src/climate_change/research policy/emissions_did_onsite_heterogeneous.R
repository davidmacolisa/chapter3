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
### Loading Scripts and Data
#======================================================================================================================#
source(file = "your_path/functions.R", echo = T)
file <- "your_path/triQc_onsite_econj.rds"
triQc <- read_rds(file = file)
#======================================================================================================================#
### Triple Differences
#======================================================================================================================#
### Financial Constraints
### I proxy financial constraints by the first pre-treatment period ratio of industries profit margin. That is, the
# ratio of industry revenue to profit, which measures the proportion of industry revenue allocated to profit.
# Industries with profit margin lower than the median profit margin are more financially constrained, and those above
# are low.
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
  ) %>%
  ungroup()

rtp_50th_percentile <- quantile(triQc[triQc$year == 2011,]$revenue.to.profit, probs = 0.5)
ptr_50th_percentile <- quantile(triQc[triQc$year == 2011,]$payroll.to.revenue, probs = 0.5)
wtr_50th_percentile <- quantile(triQc[triQc$year == 2011,]$wages.to.revenue, probs = 0.5)
profit_50th_percentile <- quantile(triQc[triQc$year == 2011,]$profit, probs = 0.5)

triQc <- triQc %>%
  mutate(
	high.profit.margin = case_when(revenue.to.profit > rtp_50th_percentile ~ 1, TRUE ~ 0),
	high.payroll.revenue = case_when(payroll.to.revenue > ptr_50th_percentile ~ 1, TRUE ~ 0),
	high.wages.revenue = case_when(wages.to.revenue > wtr_50th_percentile ~ 1, TRUE ~ 0),
	high.profit = case_when(profit > profit_50th_percentile ~ 1, TRUE ~ 0),
	labour.intensive = case_when(high.payroll.revenue == 1 & high.wages.revenue == 1 ~ 1, TRUE ~ 0),
	high.profit.labour = case_when(high.profit == 1 & labour.intensive == 1 ~ 1,
								   high.profit == 1 & labour.intensive == 0 ~ 0),
	low.profit.labour = case_when(high.profit == 0 & labour.intensive == 1 ~ 1,
								  high.profit == 0 & labour.intensive == 0 ~ 0),
  )

triQc %>% sum_up(c(high.payroll.revenue, high.wages.revenue, high.profit, labour.intensive,
				   high.profit.labour, low.profit.labour))
#======================================================================================================================#
### Onsite: High Profit and Labour Intensive vs. High Profit Capital Intensive (high profit and Technology)
#======================================================================================================================#
### Onsite: Total releases intensity (high profit and labour intensive)
#======================================================================================================================#
sdid_total_releases_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_hpli, digits = 3, digits.stats = 3)
etable(sdid_total_releases_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_total_releases_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_total_releases_hpli, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Total Onsite Releases Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_hpci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_hpci, digits = 3, digits.stats = 3)
iplot(sdid_total_releases_hpci, xlim = c(-3, 3), ylim = c(-0.35, 0.6), col = "blue",
	  main = "Total Onsite Releases Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (high profit and labour intensive)
#======================================================================================================================#
sdid_air_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_hpli, digits = 3, digits.stats = 3)
etable(sdid_air_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_air_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_air_hpli, xlim = c(-3, 3), ylim = c(-0.5, 0.65), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_hpci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_hpci, digits = 3, digits.stats = 3)
iplot(sdid_air_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (high profit and labour intensive)
#======================================================================================================================#
sdid_point_air_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_hpli, digits = 3, digits.stats = 3)
etable(sdid_point_air_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_point_air_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_point_air_hpli, xlim = c(-3, 3), ylim = c(-0.3, 0.6), col = "blue",
	  main = "Total Point Air Emissions Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_hpci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_hpci, digits = 3, digits.stats = 3)
iplot(sdid_point_air_hpci, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Fugitive air emissions intensity (high profit and labour intensive)
#======================================================================================================================#
sdid_fug_air_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_hpli, digits = 3, digits.stats = 3)
etable(sdid_fug_air_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_fug_air_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_fug_air_hpli, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_hpci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_hpci, digits = 3, digits.stats = 3)
iplot(sdid_fug_air_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Water Discharge intensity (high profit and labour intensive)
#======================================================================================================================#
sdid_water_disc_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_hpli, digits = 3, digits.stats = 3)
etable(sdid_water_disc_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_water_disc_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_water_disc_hpli, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_hpci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_hpci, digits = 3, digits.stats = 3)
iplot(sdid_water_disc_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Surface Water Discharge Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Land Releases intensity (high profit and labour intensive)
#======================================================================================================================#
sdid_land_releases_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_hpli, digits = 3, digits.stats = 3)
etable(sdid_land_releases_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_land_releases_hpli, xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_hpci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_hpci, digits = 3, digits.stats = 3)
iplot(sdid_land_releases_hpci, xlim = c(-3, 3), ylim = c(-0.05, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Impoundment intensity (high profit and labour intensive)
#======================================================================================================================#
sdid_surface_impoundment_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_hpli, digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_hpli, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
	  main = "Total Surface Impoundment Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_hpci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_hpci, digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_hpci, xlim = c(-3, 3), ylim = c(-0.03, 0.08), col = "blue",
	  main = "Total Surface Impoundment Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_highprofitlab.pdf", width =
  22,
	height = 10)
par(mfrow = c(3, 5))
iplot(sdid_total_releases_hpli, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Total Onsite Releases Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_total_releases_hpci, xlim = c(-3, 3), ylim = c(-0.35, 0.6), col = "blue",
	  main = "Total Onsite Releases Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_hpli, xlim = c(-3, 3), ylim = c(-0.5, 0.65), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_hpli, xlim = c(-3, 3), ylim = c(-0.3, 0.6), col = "blue",
	  main = "Total Point Air Emissions Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_hpci, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_hpli, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_hpli, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Surface Water Discharge Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_hpli, xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_hpci, xlim = c(-3, 3), ylim = c(-0.05, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_hpli, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
	  main = "Total Surface Impoundment Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_hpci, xlim = c(-3, 3), ylim = c(-0.03, 0.08), col = "blue",
	  main = "Total Surface Impoundment Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Onsite: Low Profit and Labour Intensive vs. Low Profit Capital Intensive (low profit and production technology)
#======================================================================================================================#
### Onsite: Total releases intensity (low profit and labour intensive)
#======================================================================================================================#
sdid_total_releases_lpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_lpli, digits = 3, digits.stats = 3)
etable(sdid_total_releases_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_total_releases_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_total_releases_lpli, xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = "blue",
	  main = "Total Onsite Releases Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_lpci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_lpci, digits = 3, digits.stats = 3)
iplot(sdid_total_releases_lpci, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (low profit and labour intensive)
#======================================================================================================================#
sdid_air_lpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_lpli, digits = 3, digits.stats = 3)
etable(sdid_air_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_air_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_air_lpli, xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_lpci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_lpci, digits = 3, digits.stats = 3)
iplot(sdid_air_lpci, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (low profit and labour intensive)
#======================================================================================================================#
sdid_point_air_lpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_lpli, digits = 3, digits.stats = 3)
etable(sdid_point_air_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_point_air_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_point_air_lpli, xlim = c(-3, 3), ylim = c(-0.5, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_lpci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_lpci, digits = 3, digits.stats = 3)
iplot(sdid_point_air_lpci, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Fugitive air emissions intensity (low profit and labour intensive)
#======================================================================================================================#
sdid_fug_air_lpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_lpli, digits = 3, digits.stats = 3)
etable(sdid_fug_air_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_fug_air_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_fug_air_lpli, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_lpci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_lpci, digits = 3, digits.stats = 3)
iplot(sdid_fug_air_lpci, xlim = c(-3, 3), ylim = c(-0.35, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Water Discharge intensity (low profit and labour intensive)
#======================================================================================================================#
sdid_water_disc_lpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_lpli, digits = 3, digits.stats = 3)
etable(sdid_water_disc_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_water_disc_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_water_disc_lpli, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_lpci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_lpci, digits = 3, digits.stats = 3)
iplot(sdid_water_disc_lpci, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Land Releases intensity (low profit and labour intensive)
#======================================================================================================================#
sdid_land_releases_lpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_lpli, digits = 3, digits.stats = 3)
etable(sdid_land_releases_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_land_releases_lpli, xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_lpci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_lpci, digits = 3, digits.stats = 3)
iplot(sdid_land_releases_lpci, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Impoundment intensity (low profit and labour intensive)
#======================================================================================================================#
sdid_surface_impoundment_lpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_lpli, digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_lpli, xlim = c(-3, 3), ylim = c(-0.02, 0.01), col = "blue",
	  main = "Total Surface Impoundment Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_lpci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_lpci, digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_lpci, xlim = c(-3, 3), ylim = c(-0.02, 0.04), col = "blue",
	  main = "Total Surface Impoundment Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_lowprofitlab.pdf", width = 22,
	height = 10)
par(mfrow = c(3, 5))
iplot(sdid_total_releases_lpli, xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = "blue",
	  main = "Total Onsite Releases Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_total_releases_lpci, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_lpli, xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_lpci, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_lpli, xlim = c(-3, 3), ylim = c(-0.5, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_lpci, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_lpli, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_lpci, xlim = c(-3, 3), ylim = c(-0.35, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_lpli, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_lpci, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_lpli, xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_lpci, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_lpli, xlim = c(-3, 3), ylim = c(-0.02, 0.01), col = "blue",
	  main = "Total Surface Impoundment Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_lpci, xlim = c(-3, 3), ylim = c(-0.02, 0.04), col = "blue",
	  main = "Total Surface Impoundment Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Type of Production Technology
#======================================================================================================================#
### Onsite: Total releases intensity (labour intensive)
#======================================================================================================================#
sdid_total_releases_li <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "labour.intensive",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_li, digits = 3, digits.stats = 3)
etable(sdid_total_releases_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_total_releases_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_total_releases_li, xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = "blue",
	  main = "Total Onsite Releases Intensity, LI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_ci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "labour.intensive",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_ci, digits = 3, digits.stats = 3)
iplot(sdid_total_releases_ci, xlim = c(-3, 3), ylim = c(-0.2, 0.5), col = "blue",
	  main = "Total Onsite Releases Intensity, CI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (labour intensive)
#======================================================================================================================#
sdid_air_li <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "labour.intensive",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_li, digits = 3, digits.stats = 3)
etable(sdid_air_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_air_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_air_li, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, LI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_ci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "labour.intensive",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_ci, digits = 3, digits.stats = 3)
iplot(sdid_air_ci, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, CI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (payroll and wages to revenue ratio)
#======================================================================================================================#
sdid_point_air_li <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "labour.intensive",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_li, digits = 3, digits.stats = 3)
etable(sdid_point_air_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_point_air_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_point_air_li, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Point Air Emissions Intensity, LI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_ci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "labour.intensive",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_ci, digits = 3, digits.stats = 3)
iplot(sdid_point_air_ci, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, CI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Fugitive air emissions intensity (payroll and wages to revenue ratio)
#======================================================================================================================#
sdid_fug_air_li <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "labour.intensive",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_li, digits = 3, digits.stats = 3)
etable(sdid_fug_air_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_fug_air_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_fug_air_li, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_ci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "labour.intensive",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_ci, digits = 3, digits.stats = 3)
iplot(sdid_fug_air_ci, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, CI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Water Discharge intensity (payroll and wages to revenue ratio)
#======================================================================================================================#
sdid_water_disc_li <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "labour.intensive",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_li, digits = 3, digits.stats = 3)
etable(sdid_water_disc_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_water_disc_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_water_disc_li, xlim = c(-3, 3), ylim = c(-0.3, 0.1), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_ci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "labour.intensive",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_ci, digits = 3, digits.stats = 3)
iplot(sdid_water_disc_ci, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Surface Water Discharge Intensity, CI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Land Releases intensity (payroll and wages to revenue ratio)
#======================================================================================================================#
sdid_land_releases_li <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "labour.intensive",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_li, digits = 3, digits.stats = 3)
etable(sdid_land_releases_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_land_releases_li, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, LI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_ci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "labour.intensive",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_ci, digits = 3, digits.stats = 3)
iplot(sdid_land_releases_ci, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, CI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Impoundment intensity (payroll and wages to revenue ratio)
#======================================================================================================================#
sdid_surface_impoundment_li <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "labour.intensive",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_li, digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_li, xlim = c(-3, 3), ylim = c(-0.05, 0.03), col = "blue",
	  main = "Total Surface Impoundment Intensity, LI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_ci <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "labour.intensive",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_ci, digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_ci, xlim = c(-3, 3), ylim = c(-0.01, 0.07), col = "blue",
	  main = "Total Surface Impoundment Intensity, CI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_labour_intensive.pdf",
	width = 22, height = 10)
par(mfrow = c(3, 5))
iplot(sdid_total_releases_li, xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = "blue",
	  main = "Total Onsite Releases Intensity, LI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_total_releases_ci, xlim = c(-3, 3), ylim = c(-0.2, 0.5), col = "blue",
	  main = "Total Onsite Releases Intensity, CI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_li, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, LI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_ci, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, CI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_li, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Point Air Emissions Intensity, LI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_ci, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, CI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_li, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_ci, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, CI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_li, xlim = c(-3, 3), ylim = c(-0.3, 0.1), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_ci, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Surface Water Discharge Intensity, CI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_li, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, LI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_ci, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, CI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_li, xlim = c(-3, 3), ylim = c(-0.05, 0.03), col = "blue",
	  main = "Total Surface Impoundment Intensity, LI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_ci, xlim = c(-3, 3), ylim = c(-0.01, 0.07), col = "blue",
	  main = "Total Surface Impoundment Intensity, CI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Profit Levels
#======================================================================================================================#
### Onsite: Total releases intensity (high profit)
#======================================================================================================================#
sdid_total_releases_hp <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "high.profit",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_hp, digits = 3, digits.stats = 3)
etable(sdid_total_releases_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_total_releases_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_total_releases_hp, xlim = c(-3, 3), ylim = c(-0.2, 0.6), col = "blue",
	  main = "Total Onsite Releases Intensity, HP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_lp <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "high.profit",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_lp, digits = 3, digits.stats = 3)
iplot(sdid_total_releases_lp, xlim = c(-3, 3), ylim = c(-0.4, 0.35), col = "blue",
	  main = "Total Onsite Releases Intensity, LP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (high profit)
#======================================================================================================================#
sdid_air_hp <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "high.profit",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_hp, digits = 3, digits.stats = 3)
etable(sdid_air_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_air_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_air_hp, xlim = c(-3, 3), ylim = c(-0.2, 0.5), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, HP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_lp <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "high.profit",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_lp, digits = 3, digits.stats = 3)
iplot(sdid_air_lp, xlim = c(-3, 3), ylim = c(-0.25, 0.35), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, LP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (high.profit)
#======================================================================================================================#
sdid_point_air_hp <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "high.profit",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_hp, digits = 3, digits.stats = 3)
etable(sdid_point_air_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_point_air_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_point_air_hp, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Point Air Emissions Intensity, HP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_lp <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "high.profit",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_lp, digits = 3, digits.stats = 3)
iplot(sdid_point_air_lp, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, LP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Fugitive air emissions intensity (high.profit)
#======================================================================================================================#
sdid_fug_air_hp <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "high.profit",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_hp, digits = 3, digits.stats = 3)
etable(sdid_fug_air_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_fug_air_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_fug_air_hp, xlim = c(-3, 3), ylim = c(-0.15, 0.3), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, HP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_lp <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "high.profit",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_lp, digits = 3, digits.stats = 3)
iplot(sdid_fug_air_lp, xlim = c(-3, 3), ylim = c(-0.15, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Water Discharge intensity (high.profit)
#======================================================================================================================#
sdid_water_disc_hp <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "high.profit",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_hp, digits = 3, digits.stats = 3)
etable(sdid_water_disc_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_water_disc_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_water_disc_hp, xlim = c(-3, 3), ylim = c(-0.25, 0.25), col = "blue",
	  main = "Total Surface Water Discharge Intensity, HP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_lp <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "high.profit",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_lp, digits = 3, digits.stats = 3)
iplot(sdid_water_disc_lp, xlim = c(-3, 3), ylim = c(-0.35, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Land Releases intensity (high.profit)
#======================================================================================================================#
sdid_land_releases_hp <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "high.profit",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_hp, digits = 3, digits.stats = 3)
etable(sdid_land_releases_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_land_releases_hp, xlim = c(-3, 3), ylim = c(-0.05, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, HP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_lp <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "high.profit",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_lp, digits = 3, digits.stats = 3)
iplot(sdid_land_releases_lp, xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = "blue",
	  main = "Total Land Releases Intensity, LP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Impoundment intensity (high.profit)
#======================================================================================================================#
sdid_surface_impoundment_hp <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "high.profit",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_hp, digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_hp, xlim = c(-3, 3), ylim = c(-0.04, 0.06), col = "blue",
	  main = "Total Surface Impoundment Intensity, HP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_lp <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "high.profit",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_lp, digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_lp, xlim = c(-3, 3), ylim = c(-0.01, 0.04), col = "blue",
	  main = "Total Surface Impoundment Intensity, LP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_high_profit.pdf",
	width = 22, height = 10)
par(mfrow = c(3, 5))
iplot(sdid_total_releases_hp, xlim = c(-3, 3), ylim = c(-0.2, 0.6), col = "blue",
	  main = "Total Onsite Releases Intensity, HP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_total_releases_lp, xlim = c(-3, 3), ylim = c(-0.4, 0.35), col = "blue",
	  main = "Total Onsite Releases Intensity, LP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_hp, xlim = c(-3, 3), ylim = c(-0.2, 0.5), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, HP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_lp, xlim = c(-3, 3), ylim = c(-0.25, 0.35), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, LP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_lp, xlim = c(-3, 3), ylim = c(-0.25, 0.35), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, LP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_hp, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Point Air Emissions Intensity, HP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_lp, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, LP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_hp, xlim = c(-3, 3), ylim = c(-0.15, 0.3), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, HP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_lp, xlim = c(-3, 3), ylim = c(-0.15, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_hp, xlim = c(-3, 3), ylim = c(-0.25, 0.25), col = "blue",
	  main = "Total Surface Water Discharge Intensity, HP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_lp, xlim = c(-3, 3), ylim = c(-0.35, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_hp, xlim = c(-3, 3), ylim = c(-0.05, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, HP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_lp, xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = "blue",
	  main = "Total Land Releases Intensity, LP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_hp, xlim = c(-3, 3), ylim = c(-0.04, 0.06), col = "blue",
	  main = "Total Surface Impoundment Intensity, HP", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_lp, xlim = c(-3, 3), ylim = c(-0.01, 0.04), col = "blue",
	  main = "Total Surface Impoundment Intensity, LP", xlab = "relative year",
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
sdid_total_releases_highemitt <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "highest.emitt.ind",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_highemitt, digits = 3, digits.stats = 3)
etable(sdid_total_releases_highemitt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_total_releases_highemitt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_total_releases_highemitt, xlim = c(-3, 3), ylim = c(-0.3, 0.7), col = "blue",
	  main = "Total Onsite Releases Intensity, HEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_lowemitt <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "highest.emitt.ind",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_lowemitt, digits = 3, digits.stats = 3)
iplot(sdid_total_releases_lowemitt, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Onsite Releases Intensity, LEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity
#======================================================================================================================#
sdid_air_highemitt <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "highest.emitt.ind",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_highemitt, digits = 3, digits.stats = 3)
etable(sdid_air_highemitt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_air_highemitt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_air_highemitt, xlim = c(-3, 3), ylim = c(-0.2, 0.6), col = "blue",
	  main = "Total Air Emissions Intensity, HEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_lowemitt <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "highest.emitt.ind",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_lowemitt, digits = 3, digits.stats = 3)
iplot(sdid_air_lowemitt, xlim = c(-3, 3), ylim = c(-0.15, 0.25), col = "blue",
	  main = "Total Air Emissions Intensity, LEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity
#======================================================================================================================#
sdid_point_air_highemitt <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "highest.emitt.ind",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_highemitt, digits = 3, digits.stats = 3)
etable(sdid_point_air_highemitt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_point_air_highemitt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_point_air_highemitt, xlim = c(-3, 3), ylim = c(-0.25, 0.5), col = "blue",
	  main = "Total Point Air Emissions Intensity, HEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_lowemitt <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "highest.emitt.ind",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_lowemitt, digits = 3, digits.stats = 3)
iplot(sdid_point_air_lowemitt, xlim = c(-3, 3), ylim = c(-0.2, 0.25), col = "blue",
	  main = "Total Point Air Emissions Intensity, LEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity
#======================================================================================================================#
sdid_fug_air_highemitt <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "highest.emitt.ind",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_highemitt, digits = 3, digits.stats = 3)
etable(sdid_fug_air_highemitt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_fug_air_highemitt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_fug_air_highemitt, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, HEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_lowemitt <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "highest.emitt.ind",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_lowemitt, digits = 3, digits.stats = 3)
iplot(sdid_fug_air_lowemitt, xlim = c(-3, 3), ylim = c(-0.15, 0.2), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity
#======================================================================================================================#
sdid_water_disc_highemitt <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "highest.emitt.ind",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_highemitt, digits = 3, digits.stats = 3)
etable(sdid_water_disc_highemitt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_water_disc_highemitt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_water_disc_highemitt, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
	  main = "Total Surface Water Discharge Intensity, HEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_lowemitt <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "highest.emitt.ind",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_lowemitt, digits = 3, digits.stats = 3)
iplot(sdid_water_disc_lowemitt, xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Land Releases intensity
#======================================================================================================================#
sdid_land_releases_highemitt <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "highest.emitt.ind",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_highemitt, digits = 3, digits.stats = 3)
etable(sdid_land_releases_highemitt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases_highemitt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_land_releases_highemitt, xlim = c(-3, 3), ylim = c(-0.1, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, HEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_lowemitt <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "highest.emitt.ind",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_lowemitt, digits = 3, digits.stats = 3)
iplot(sdid_land_releases_lowemitt, xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = "blue",
	  main = "Total Land Releases Intensity, LEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Impoundment intensity
#======================================================================================================================#
sdid_surface_impound_highemitt <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "highest.emitt.ind",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impound_highemitt, digits = 3, digits.stats = 3)
etable(sdid_surface_impound_highemitt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_surface_impound_highemitt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_surface_impound_highemitt, xlim = c(-3, 3), ylim = c(-0.02, 0.02), col = "blue",
	  main = "Total Surface Impoundment Intensity, HEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impound_lowemitt <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "highest.emitt.ind",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impound_lowemitt, digits = 3, digits.stats = 3)
iplot(sdid_surface_impound_lowemitt, xlim = c(-3, 3), ylim = c(-0.01, 0.05), col = "blue",
	  main = "Total Surface Impoundment Intensity, LEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_EMITT.pdf", width = 22,
	height = 10)
par(mfrow = c(3, 5))
iplot(sdid_total_releases_highemitt, xlim = c(-3, 3), ylim = c(-0.3, 0.7), col = "blue",
	  main = "Total Onsite Releases Intensity, HEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_total_releases_lowemitt, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Onsite Releases Intensity, LEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_highemitt, xlim = c(-3, 3), ylim = c(-0.2, 0.6), col = "blue",
	  main = "Total Air Emissions Intensity, HEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_lowemitt, xlim = c(-3, 3), ylim = c(-0.15, 0.25), col = "blue",
	  main = "Total Air Emissions Intensity, LEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_highemitt, xlim = c(-3, 3), ylim = c(-0.25, 0.5), col = "blue",
	  main = "Total Point Air Emissions Intensity, HEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_lowemitt, xlim = c(-3, 3), ylim = c(-0.2, 0.25), col = "blue",
	  main = "Total Point Air Emissions Intensity, LEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_highemitt, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, HEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_lowemitt, xlim = c(-3, 3), ylim = c(-0.15, 0.2), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_highemitt, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
	  main = "Total Surface Water Discharge Intensity, HEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_lowemitt, xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_highemitt, xlim = c(-3, 3), ylim = c(-0.1, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, HEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_lowemitt, xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = "blue",
	  main = "Total Land Releases Intensity, LEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impound_highemitt, xlim = c(-3, 3), ylim = c(-0.02, 0.02), col = "blue",
	  main = "Total Surface Impoundment Intensity, HEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impound_lowemitt, xlim = c(-3, 3), ylim = c(-0.01, 0.05), col = "blue",
	  main = "Total Surface Impoundment Intensity, LEIs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Economic Growth Patterns: GDP
### Higher GDP counties are above the median GDP, and low GDP counties are below the median GDP.
#======================================================================================================================#
gdp_50th_percentile <- quantile(triQc[triQc$year == 2011,]$gdp, probs = 0.5)
triQc$high.gdp <- ifelse(triQc$gdp > gdp_50th_percentile, yes = 1, no = 0)
prop.table(table(triQc$high.gdp)) * 100
#======================================================================================================================#
### Onsite: Total releases intensity
#======================================================================================================================#
sdid_total_releases_hgdp <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "high.gdp",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_hgdp, digits = 3, digits.stats = 3)
etable(sdid_total_releases_hgdp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_total_releases_hgdp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_total_releases_hgdp, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, high GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_lgdp <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "high.gdp",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_lgdp, digits = 3, digits.stats = 3)
iplot(sdid_total_releases_lgdp, xlim = c(-3, 3), ylim = c(-0.3, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, low GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity
#======================================================================================================================#
sdid_air_hgdp <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "high.gdp",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_hgdp, digits = 3, digits.stats = 3)
etable(sdid_air_hgdp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_air_hgdp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_air_hgdp, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, high GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_lgdp <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "high.gdp",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_lgdp, digits = 3, digits.stats = 3)
iplot(sdid_air_lgdp, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, low GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity
#======================================================================================================================#
sdid_point_air_hgdp <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "high.gdp",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_hgdp, digits = 3, digits.stats = 3)
etable(sdid_point_air_hgdp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_point_air_hgdp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_point_air_hgdp, xlim = c(-3, 3), ylim = c(-0.3, 0.4), col = "blue",
	  main = "Total Point Air Emissions Intensity, high GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_lgdp <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "high.gdp",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_lgdp, digits = 3, digits.stats = 3)
iplot(sdid_point_air_lgdp, xlim = c(-3, 3), ylim = c(-0.2, 0.25), col = "blue",
	  main = "Total Point Air Emissions Intensity, low GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity
#======================================================================================================================#
sdid_fug_air_hgdp <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "high.gdp",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_hgdp, digits = 3, digits.stats = 3)
etable(sdid_fug_air_hgdp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_fug_air_hgdp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_fug_air_hgdp, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, high GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_lgdp <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "high.gdp",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_lgdp, digits = 3, digits.stats = 3)
iplot(sdid_fug_air_lgdp, xlim = c(-3, 3), ylim = c(-0.1, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, low GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity
#======================================================================================================================#
sdid_water_disc_hgdp <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "high.gdp",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_hgdp, digits = 3, digits.stats = 3)
etable(sdid_water_disc_hgdp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_water_disc_hgdp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_water_disc_hgdp, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, high GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_lgdp <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "high.gdp",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_lgdp, digits = 3, digits.stats = 3)
iplot(sdid_water_disc_lgdp, xlim = c(-3, 3), ylim = c(-0.25, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, low GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity
#======================================================================================================================#
sdid_land_releases_hgdp <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "high.gdp",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_hgdp, digits = 3, digits.stats = 3)
etable(sdid_land_releases_hgdp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases_hgdp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_land_releases_hgdp, xlim = c(-3, 3), ylim = c(-0.3, 0.08), col = "blue",
	  main = "Total Onsite Land Releases Intensity, high GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_lgdp <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "high.gdp",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_lgdp, digits = 3, digits.stats = 3)
iplot(sdid_land_releases_lgdp, xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = "blue",
	  main = "Total Onsite Land Releases Intensity, low GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity: Surface impoundment
#======================================================================================================================#
sdid_surface_impound_hgdp <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "high.gdp",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impound_hgdp, digits = 3, digits.stats = 3)
etable(sdid_surface_impound_hgdp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_surface_impound_hgdp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_surface_impound_hgdp, xlim = c(-3, 3), ylim = c(-0.01, 0.015), col = "blue",
	  main = "Total Surface Impoundment Intensity, high GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impound_lgdp <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "high.gdp",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impound_lgdp, digits = 3, digits.stats = 3)
iplot(sdid_surface_impound_lgdp, xlim = c(-3, 3), ylim = c(-0.01, 0.04), col = "blue",
	  main = "Total Surface Impoundment Intensity, low GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_GDP.pdf", width = 22,
	height = 10)
par(mfrow = c(3, 5))
iplot(sdid_total_releases_hgdp, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, high GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_total_releases_lgdp, xlim = c(-3, 3), ylim = c(-0.3, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, low GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_hgdp, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, high GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_lgdp, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, low GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_hgdp, xlim = c(-3, 3), ylim = c(-0.3, 0.4), col = "blue",
	  main = "Total Point Air Emissions Intensity, high GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_lgdp, xlim = c(-3, 3), ylim = c(-0.2, 0.25), col = "blue",
	  main = "Total Point Air Emissions Intensity, low GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_hgdp, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, high GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_lgdp, xlim = c(-3, 3), ylim = c(-0.1, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, low GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_hgdp, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, high GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_lgdp, xlim = c(-3, 3), ylim = c(-0.25, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, low GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_hgdp, xlim = c(-3, 3), ylim = c(-0.3, 0.08), col = "blue",
	  main = "Total Onsite Land Releases Intensity, high GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_lgdp, xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = "blue",
	  main = "Total Onsite Land Releases Intensity, low GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impound_hgdp, xlim = c(-3, 3), ylim = c(-0.01, 0.015), col = "blue",
	  main = "Total Surface Impoundment Intensity, high GDP counties", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impound_lgdp, xlim = c(-3, 3), ylim = c(-0.01, 0.04), col = "blue",
	  main = "Total Surface Impoundment Intensity, low GDP counties", xlab = "relative year",
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

# low HHI, low industry concentration and more competition
q_hhi <- quantile(triQc[triQc$year == 2011,]$hhi, probs = 0.5)
triQc$low.ind.conc <- as.numeric(triQc$hhi < q_hhi)
sum_up(triQc, c(revenue, revenue.2, sum.revenue.2, hhi, low.ind.conc))
table(triQc$low.ind.conc)
#======================================================================================================================#
### Onsite: Total releases intensity (industry concentration)
#======================================================================================================================#
sdid_total_releases_lowindconc <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "low.ind.conc",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_lowindconc, digits = 3, digits.stats = 3)
etable(sdid_total_releases_lowindconc, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_total_releases_lowindconc, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_total_releases_lowindconc, xlim = c(-3, 3), ylim = c(-0.4, 0.25), col = "blue",
	  main = "Total Onsite Releases Intensity, LCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_highindconc <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "low.ind.conc",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_highindconc, digits = 3, digits.stats = 3)
iplot(sdid_total_releases_highindconc, xlim = c(-3, 3), ylim = c(-0.25, 0.3), col = "blue",
	  main = "Total Onsite Releases Intensity, HCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (industry concentration)
#======================================================================================================================#
sdid_air_lowindconc <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "low.ind.conc",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_lowindconc, digits = 3, digits.stats = 3)
etable(sdid_air_lowindconc, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_air_lowindconc, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_air_lowindconc, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Air Emissions Intensity, LCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_highindconc <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "low.ind.conc",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_highindconc, digits = 3, digits.stats = 3)
iplot(sdid_air_highindconc, xlim = c(-3, 3), ylim = c(-0.15, 0.3), col = "blue",
	  main = "Total Air Emissions Intensity, HCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (industry concentration)
#======================================================================================================================#
sdid_point_air_lowindconc <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "low.ind.conc",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_lowindconc, digits = 3, digits.stats = 3)
etable(sdid_point_air_lowindconc, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_point_air_lowindconc, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_point_air_lowindconc, xlim = c(-3, 3), ylim = c(-0.35, 0.15), col = "blue",
	  main = "Total Point Air Emissions Intensity, LCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_highindconc <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "low.ind.conc",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_highindconc, digits = 3, digits.stats = 3)
iplot(sdid_point_air_highindconc, xlim = c(-3, 3), ylim = c(-0.25, 0.25), col = "blue",
	  main = "Total Point Air Emissions Intensity, HCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (industry concentration)
#======================================================================================================================#
sdid_fug_air_lowindconc <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "low.ind.conc",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_lowindconc, digits = 3, digits.stats = 3)
etable(sdid_fug_air_lowindconc, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_fug_air_lowindconc, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_fug_air_lowindconc, xlim = c(-3, 3), ylim = c(-0.2, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_highindconc <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "low.ind.conc",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_highindconc, digits = 3, digits.stats = 3)
iplot(sdid_fug_air_highindconc, xlim = c(-3, 3), ylim = c(-0.15, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, HCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (industry concentration)
#======================================================================================================================#
sdid_water_disc_lowindconc <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "low.ind.conc",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_lowindconc, digits = 3, digits.stats = 3)
etable(sdid_water_disc_lowindconc, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_water_disc_lowindconc, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_water_disc_lowindconc, xlim = c(-3, 3), ylim = c(-0.25, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_highindconc <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "low.ind.conc",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_highindconc, digits = 3, digits.stats = 3)
iplot(sdid_water_disc_highindconc, xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = "blue",
	  main = "Total Surface Water Discharge Intensity, HCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity (industry concentration)
#======================================================================================================================#
sdid_land_releases_lowindconc <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "low.ind.conc",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_lowindconc, digits = 3, digits.stats = 3)
etable(sdid_land_releases_lowindconc, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases_lowindconc, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_land_releases_lowindconc, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, LCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_highindconc <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "low.ind.conc",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_highindconc, digits = 3, digits.stats = 3)
iplot(sdid_land_releases_highindconc, xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = "blue",
	  main = "Total Land Releases Intensity, HCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (industry concentration)
#======================================================================================================================#
sdid_surface_impoundment_lowindconc <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "low.ind.conc",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_lowindconc, digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_lowindconc, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_lowindconc, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_lowindconc, xlim = c(-3, 3), ylim = c(-0.02, 0.03), col = "blue",
	  main = "Total Surface Impoundment Intensity, LCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_highindconc <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "low.ind.conc",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_highindconc, digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_highindconc, xlim = c(-3, 3), ylim = c(-0.01, 0.04), col = "blue",
	  main = "Total Surface Impoundment Intensity, HCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_lowindconc.pdf",
	width = 20, height = 10)
par(mfrow = c(3, 5))
iplot(sdid_total_releases_lowindconc, xlim = c(-3, 3), ylim = c(-0.4, 0.25), col = "blue",
	  main = "Total Onsite Releases Intensity, LCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_total_releases_highindconc, xlim = c(-3, 3), ylim = c(-0.25, 0.3), col = "blue",
	  main = "Total Onsite Releases Intensity, HCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_lowindconc, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Air Emissions Intensity, LCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_highindconc, xlim = c(-3, 3), ylim = c(-0.15, 0.3), col = "blue",
	  main = "Total Air Emissions Intensity, HCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_lowindconc, xlim = c(-3, 3), ylim = c(-0.35, 0.15), col = "blue",
	  main = "Total Point Air Emissions Intensity, LCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_highindconc, xlim = c(-3, 3), ylim = c(-0.25, 0.25), col = "blue",
	  main = "Total Point Air Emissions Intensity, HCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_lowindconc, xlim = c(-3, 3), ylim = c(-0.2, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_highindconc, xlim = c(-3, 3), ylim = c(-0.15, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, HCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_lowindconc, xlim = c(-3, 3), ylim = c(-0.25, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_highindconc, xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = "blue",
	  main = "Total Surface Water Discharge Intensity, HCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_lowindconc, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, LCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_highindconc, xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = "blue",
	  main = "Total Land Releases Intensity, HCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_lowindconc, xlim = c(-3, 3), ylim = c(-0.02, 0.03), col = "blue",
	  main = "Total Surface Impoundment Intensity, LCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_highindconc, xlim = c(-3, 3), ylim = c(-0.01, 0.04), col = "blue",
	  main = "Total Surface Impoundment Intensity, HCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Is the MW Policy potentially Carcinogenic?
#======================================================================================================================#
### Onsite: Total releases intensity (carcinonogenic chemicals)
#======================================================================================================================#
sdid_total_releases_carcinogenic <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "carcinogenic.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_carcinogenic, digits = 3, digits.stats = 3)
etable(sdid_total_releases_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_total_releases_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_total_releases_carcinogenic, xlim = c(-3, 3), ylim = c(-0.9, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_noncarcinogenic <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "carcinogenic.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_noncarcinogenic, digits = 3, digits.stats = 3)
iplot(sdid_total_releases_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.25, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, Non-Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (carcinogenic chemicals)
#======================================================================================================================#
sdid_air_carcinogenic <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "carcinogenic.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_carcinogenic, digits = 3, digits.stats = 3)
etable(sdid_air_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_air_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_air_carcinogenic, xlim = c(-3, 3), ylim = c(-0.9, 0.4), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_noncarcinogenic <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "carcinogenic.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_noncarcinogenic, digits = 3, digits.stats = 3)
iplot(sdid_air_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.2, 0.25), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (carcinogenic chemicals)
#======================================================================================================================#
sdid_point_air_carcinogenic <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "carcinogenic.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_carcinogenic, digits = 3, digits.stats = 3)
etable(sdid_point_air_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_point_air_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_point_air_carcinogenic, xlim = c(-3, 3), ylim = c(-0.95, 0.4), col = "blue",
	  main = "Total Point Air Emissions Intensity, Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_noncarcinogenic <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "carcinogenic.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_noncarcinogenic, digits = 3, digits.stats = 3)
iplot(sdid_point_air_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Point Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (carcinogenic chemicals)
#======================================================================================================================#
sdid_fug_air_carcinogenic <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "carcinogenic.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_carcinogenic, digits = 3, digits.stats = 3)
etable(sdid_fug_air_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_fug_air_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_fug_air_carcinogenic, xlim = c(-3, 3), ylim = c(-0.6, 0.4), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_noncarcinogenic <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "carcinogenic.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_noncarcinogenic, digits = 3, digits.stats = 3)
iplot(sdid_fug_air_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.15, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (carcinogenic chemicals)
#======================================================================================================================#
sdid_water_disc_carcinogenic <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "carcinogenic.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_carcinogenic, digits = 3, digits.stats = 3)
etable(sdid_water_disc_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_water_disc_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_water_disc_carcinogenic, xlim = c(-3, 3), ylim = c(-0.5, 0.25), col = "blue",
	  main = "Total Surface Water Discharge Intensity, Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_noncarcinogenic <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "carcinogenic.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_noncarcinogenic, digits = 3, digits.stats = 3)
iplot(sdid_water_disc_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, Non-Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity (carcinogenic chemicals)
#======================================================================================================================#
sdid_land_releases_carcinogenic <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "carcinogenic.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_carcinogenic, digits = 3, digits.stats = 3)
etable(sdid_land_releases_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_land_releases_carcinogenic, xlim = c(-3, 3), ylim = c(-0.25, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_noncarcinogenic <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "carcinogenic.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_noncarcinogenic, digits = 3, digits.stats = 3)
iplot(sdid_land_releases_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, Non-Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (carcinogenic chemicals)
#======================================================================================================================#
sdid_surface_impoundment_carcinogenic <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "carcinogenic.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_carcinogenic, digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_carcinogenic, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_carcinogenic, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_carcinogenic, xlim = c(-3, 3), ylim = c(-0.04, 0.02), col = "blue",
	  main = "Total Surface Impoundment Intensity, Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_noncarcinogenic <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "carcinogenic.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_noncarcinogenic, digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.01, 0.05), col = "blue",
	  main = "Total Surface Impoundment Intensity, Non-Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_carcinogens.pdf",
	width = 20, height = 10)
par(mfrow = c(3, 5))
iplot(sdid_total_releases_carcinogenic, xlim = c(-3, 3), ylim = c(-0.9, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_total_releases_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.25, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, Non-Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_carcinogenic, xlim = c(-3, 3), ylim = c(-0.9, 0.4), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.2, 0.25), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_carcinogenic, xlim = c(-3, 3), ylim = c(-0.95, 0.4), col = "blue",
	  main = "Total Point Air Emissions Intensity, Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Point Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_carcinogenic, xlim = c(-3, 3), ylim = c(-0.6, 0.4), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.15, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, Non-Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_carcinogenic, xlim = c(-3, 3), ylim = c(-0.5, 0.25), col = "blue",
	  main = "Total Surface Water Discharge Intensity, Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, Non-Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_carcinogenic, xlim = c(-3, 3), ylim = c(-0.25, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, Non-Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_carcinogenic, xlim = c(-3, 3), ylim = c(-0.04, 0.02), col = "blue",
	  main = "Total Surface Impoundment Intensity, Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_noncarcinogenic, xlim = c(-3, 3), ylim = c(-0.01, 0.05), col = "blue",
	  main = "Total Surface Impoundment Intensity, Non-Carcinogens", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Does the MW Policy affect emissions intensities of common clean air act and haps regulated chemicals?
#======================================================================================================================#
triQc <- triQc %>%
  mutate(caa.haps = ifelse(test = clean.air.act.chems == 1 & hap.chems == 1, yes = 1, no = 0))
table(triQc$caa.haps)

non_caahaps <- triQc %>%
  filter(caa.haps == 0) %>%
  distinct(chemical.name) %>%
  pull(chemical.name)
#======================================================================================================================#
### Onsite: Total releases intensity (clean air act and haps regulated chemicals)
#======================================================================================================================#
sdid_total_releases_caahaps <- sdid_releases_heter_epa_1(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "caa.haps",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_caahaps, digits = 3, digits.stats = 3)
etable(sdid_total_releases_caahaps, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_total_releases_caahaps, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_total_releases_caahaps, xlim = c(-3, 3), ylim = c(-0.8, 0.6), col = "blue",
	  main = "Total Onsite Releases Intensity, common-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_noncaahaps <- sdid_releases_heter_epa_0(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "caa.haps",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_noncaahaps, digits = 3, digits.stats = 3)
iplot(sdid_total_releases_noncaahaps, xlim = c(-3, 3), ylim = c(-0.3, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, uncommon-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (clean air act and haps regulated chemicals)
#======================================================================================================================#
sdid_air_caahaps <- sdid_releases_heter_epa_1(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "caa.haps",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_caahaps, digits = 3, digits.stats = 3)
etable(sdid_air_caahaps, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_air_caahaps, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_air_caahaps, xlim = c(-3, 3), ylim = c(-0.7, 0.7), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, common-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_noncaahaps <- sdid_releases_heter_epa_0(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "caa.haps",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_noncaahaps, digits = 3, digits.stats = 3)
iplot(sdid_air_noncaahaps, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, uncommon-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (clean air act and haps regulated chemicals)
#======================================================================================================================#
sdid_point_air_caahaps <- sdid_releases_heter_epa_1(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "caa.haps",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_caahaps, digits = 3, digits.stats = 3)
etable(sdid_point_air_caahaps, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_point_air_caahaps, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_point_air_caahaps, xlim = c(-3, 3), ylim = c(-0.8, 0.6), col = "blue",
	  main = "Total Point Air Emissions Intensity, common-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_noncaahaps <- sdid_releases_heter_epa_0(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "caa.haps",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_noncaahaps, digits = 3, digits.stats = 3)
iplot(sdid_point_air_noncaahaps, xlim = c(-3, 3), ylim = c(-0.3, 0.25), col = "blue",
	  main = "Total Point Air Emissions Intensity, uncommon-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (clean air act and haps regulated chemicals)
#======================================================================================================================#
sdid_fug_air_caahaps <- sdid_releases_heter_epa_1(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "caa.haps",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_caahaps, digits = 3, digits.stats = 3)
etable(sdid_fug_air_caahaps, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_fug_air_caahaps, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_fug_air_caahaps, xlim = c(-3, 3), ylim = c(-0.4, 1.1), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, common-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_noncaahaps <- sdid_releases_heter_epa_0(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "caa.haps",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_noncaahaps, digits = 3, digits.stats = 3)
iplot(sdid_fug_air_noncaahaps, xlim = c(-3, 3), ylim = c(-0.15, 0.4), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, uncommon-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (clean air act and haps regulated chemicals)
#======================================================================================================================#
sdid_water_disc_caahaps <- sdid_releases_heter_epa_1(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "caa.haps",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_caahaps, digits = 3, digits.stats = 3)
etable(sdid_water_disc_caahaps, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_water_disc_caahaps, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_water_disc_caahaps, xlim = c(-3, 3), ylim = c(-0.7, 0.4), col = "blue",
	  main = "Total Surface Water Discharge Intensity, common-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_noncaahaps <- sdid_releases_heter_epa_0(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "caa.haps",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_noncaahaps, digits = 3, digits.stats = 3)
iplot(sdid_water_disc_noncaahaps, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Surface Water Discharge Intensity, uncommon-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity (clean air act and haps regulated chemicals)
#======================================================================================================================#
sdid_land_releases_caahaps <- sdid_releases_heter_epa_1(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "caa.haps",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_caahaps, digits = 3, digits.stats = 3)
etable(sdid_land_releases_caahaps, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases_caahaps, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_land_releases_caahaps, xlim = c(-3, 3), ylim = c(-0.4, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, common-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_noncaahaps <- sdid_releases_heter_epa_0(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "caa.haps",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_noncaahaps, digits = 3, digits.stats = 3)
iplot(sdid_land_releases_noncaahaps, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, uncommon-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (clean air act and haps regulated chemicals)
#======================================================================================================================#
sdid_surface_impoundment_caahaps <- sdid_releases_heter_epa_1(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "caa.haps",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_caahaps, digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_caahaps, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_caahaps, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_caahaps, xlim = c(-3, 3), ylim = c(-0.03, 0.03), col = "blue",
	  main = "Total Surface Impoundment Intensity, common-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_noncaahaps <- sdid_releases_heter_epa_0(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "caa.haps",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_noncaahaps, digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_noncaahaps, xlim = c(-3, 3), ylim = c(-0.01, 0.04), col = "blue",
	  main = "Total Surface Impoundment Intensity, uncommon-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_caahaps.pdf", width = 23,
	height = 10)
par(mfrow = c(3, 5))
iplot(sdid_total_releases_caahaps, xlim = c(-3, 3), ylim = c(-0.8, 0.6), col = "blue",
	  main = "Total Onsite Releases Intensity, common-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_total_releases_noncaahaps, xlim = c(-3, 3), ylim = c(-0.3, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, uncommon-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_caahaps, xlim = c(-3, 3), ylim = c(-0.7, 0.7), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, common-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_noncaahaps, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, uncommon-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_noncaahaps, xlim = c(-3, 3), ylim = c(-0.3, 0.25), col = "blue",
	  main = "Total Point Air Emissions Intensity, uncommon-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_caahaps, xlim = c(-3, 3), ylim = c(-0.4, 1.1), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, common-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_noncaahaps, xlim = c(-3, 3), ylim = c(-0.15, 0.4), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, uncommon-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_caahaps, xlim = c(-3, 3), ylim = c(-0.7, 0.4), col = "blue",
	  main = "Total Surface Water Discharge Intensity, common-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_noncaahaps, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Surface Water Discharge Intensity, uncommon-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_caahaps, xlim = c(-3, 3), ylim = c(-0.4, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, common-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_noncaahaps, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, uncommon-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_caahaps, xlim = c(-3, 3), ylim = c(-0.03, 0.03), col = "blue",
	  main = "Total Surface Impoundment Intensity, common-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_noncaahaps, xlim = c(-3, 3), ylim = c(-0.01, 0.04), col = "blue",
	  main = "Total Surface Impoundment Intensity, uncommon-CAA-HAPs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Does the MW Policy affect emissions intensity of the Persistent Bioaccumulative Toxic Chemicals?
#======================================================================================================================#
### Onsite: Total releases intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
sdid_total_releases_pbt <- sdid_releases_heter_epa_1(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "pbt.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_pbt, digits = 3, digits.stats = 3)
etable(sdid_total_releases_pbt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_total_releases_pbt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_total_releases_pbt, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
	  main = "Total Onsite Releases Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_nonpbt <- sdid_releases_heter_epa_0(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "pbt.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_nonpbt, digits = 3, digits.stats = 3)
iplot(sdid_total_releases_nonpbt, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, Non-PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
sdid_air_pbt <- sdid_releases_heter_epa_1(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "pbt.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_pbt, digits = 3, digits.stats = 3)
etable(sdid_air_pbt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_air_pbt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_air_pbt, xlim = c(-3, 3), ylim = c(-0.8, 0.7), col = "blue",
	  main = "Total Air Emissions Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_nonpbt <- sdid_releases_heter_epa_0(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "pbt.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_nonpbt, digits = 3, digits.stats = 3)
iplot(sdid_air_nonpbt, xlim = c(-3, 3), ylim = c(-0.15, 0.35), col = "blue",
	  main = "Total Air Emissions Intensity, Non-PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
sdid_point_air_pbt <- sdid_releases_heter_epa_1(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "pbt.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_pbt, digits = 3, digits.stats = 3)
etable(sdid_point_air_pbt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_point_air_pbt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_point_air_pbt, xlim = c(-3, 3), ylim = c(-0.9, 0.7), col = "blue",
	  main = "Total Point Air Emissions Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_nonpbt <- sdid_releases_heter_epa_0(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "pbt.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_nonpbt, digits = 3, digits.stats = 3)
iplot(sdid_point_air_nonpbt, xlim = c(-3, 3), ylim = c(-0.15, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, Non-PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
sdid_fug_air_pbt <- sdid_releases_heter_epa_1(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "pbt.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_pbt, digits = 3, digits.stats = 3)
etable(sdid_fug_air_pbt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_fug_air_pbt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_fug_air_pbt, xlim = c(-3, 3), ylim = c(-0.6, 0.5), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_nonpbt <- sdid_releases_heter_epa_0(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "pbt.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_nonpbt, digits = 3, digits.stats = 3)
iplot(sdid_fug_air_nonpbt, xlim = c(-3, 3), ylim = c(-0.15, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, Non-PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface water discharge intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
sdid_water_disc_pbt <- sdid_releases_heter_epa_1(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "pbt.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_pbt, digits = 3, digits.stats = 3)
etable(sdid_water_disc_pbt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_water_disc_pbt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_water_disc_pbt, xlim = c(-3, 3), ylim = c(-0.25, 0.6), col = "blue",
	  main = "Total Surface Water Discharge Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_nonpbt <- sdid_releases_heter_epa_0(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "pbt.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_nonpbt, digits = 3, digits.stats = 3)
iplot(sdid_water_disc_nonpbt, xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = "blue",
	  main = "Total Surface Water Discharge Intensity, Non-PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total land releases intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
sdid_land_releases_pbt <- sdid_releases_heter_epa_1(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "pbt.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_pbt, digits = 3, digits.stats = 3)
etable(sdid_land_releases_pbt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases_pbt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_land_releases_pbt, xlim = c(-3, 3), ylim = c(-0.25, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_nonpbt <- sdid_releases_heter_epa_0(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "pbt.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_nonpbt, digits = 3, digits.stats = 3)
iplot(sdid_land_releases_nonpbt, xlim = c(-3, 3), ylim = c(-0.15, 0.06), col = "blue",
	  main = "Total Land Releases Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total surface impoundment intensity (Persistent Bioaccumulative Toxic Chemicals)
#======================================================================================================================#
sdid_surface_impoundment_pbt <- sdid_releases_heter_epa_1(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "pbt.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_pbt, digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_pbt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_pbt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_pbt, xlim = c(-3, 3), ylim = c(-0.04, 0.04), col = "blue",
	  main = "Total Surface Impoundment Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_nonpbt <- sdid_releases_heter_epa_0(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "pbt.chems",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_nonpbt, digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_nonpbt, xlim = c(-3, 3), ylim = c(-0.01, 0.042), col = "blue",
	  main = "Total Surface Impoundment Intensity, Non-PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_pbts.pdf", width = 22,
	height = 10)
par(mfrow = c(3, 5))
iplot(sdid_total_releases_pbt, xlim = c(-3, 3), ylim = c(-0.9, 0.9), col = "blue",
	  main = "Total Onsite Releases Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_total_releases_nonpbt, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, Non-PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_pbt, xlim = c(-3, 3), ylim = c(-0.8, 0.7), col = "blue",
	  main = "Total Air Emissions Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_nonpbt, xlim = c(-3, 3), ylim = c(-0.15, 0.35), col = "blue",
	  main = "Total Air Emissions Intensity, Non-PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_pbt, xlim = c(-3, 3), ylim = c(-0.9, 0.7), col = "blue",
	  main = "Total Point Air Emissions Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_nonpbt, xlim = c(-3, 3), ylim = c(-0.15, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, Non-PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_pbt, xlim = c(-3, 3), ylim = c(-0.6, 0.5), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_nonpbt, xlim = c(-3, 3), ylim = c(-0.15, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, Non-PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_pbt, xlim = c(-3, 3), ylim = c(-0.25, 0.6), col = "blue",
	  main = "Total Surface Water Discharge Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_nonpbt, xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = "blue",
	  main = "Total Surface Water Discharge Intensity, Non-PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_pbt, xlim = c(-3, 3), ylim = c(-0.25, 0.1), col = "blue",
	  main = "Total Land Releases Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_nonpbt, xlim = c(-3, 3), ylim = c(-0.15, 0.06), col = "blue",
	  main = "Total Land Releases Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_pbt, xlim = c(-3, 3), ylim = c(-0.04, 0.04), col = "blue",
	  main = "Total Surface Impoundment Intensity, PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_nonpbt, xlim = c(-3, 3), ylim = c(-0.01, 0.042), col = "blue",
	  main = "Total Surface Impoundment Intensity, Non-PBTs", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
corr_plot <- cor(x = triQc %>% select(c(
  high.profit, high.payroll.revenue, high.wages.revenue, labour.intensive,
  highest.emitt.ind, high.gdp, low.ind.conc, carcinogenic.chems, clean.air.act.chems, hap.chems,
  pbt.chems, caa.haps, low.skilled.workers
)), method = "pearson")

correlation_het <- corrplot::corrplot(
  corr = corr_plot,
  method = "circle",
  type = "lower",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = TRUE
)

pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_correlation_het.pdf", width = 10, height = 10)
corrplot::corrplot(
  corr = corr_plot,
  method = "circle",
  type = "lower",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = TRUE
)
dev.off()
#======================================================================================================================#