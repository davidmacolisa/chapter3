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
source(file = "./Thesis/chapter3/src/climate_change/codes/functions.R", echo = T)
file <- "./Data_PhD/US/BLS/onsite/triQc_on.rds"
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
	high.profit.capital = case_when(high.profit == 1 & labour.intensive == 0 ~ 1,
									high.profit == 1 & labour.intensive == 1 ~ 0),
	low.profit.capital = case_when(high.profit == 0 & labour.intensive == 0 ~ 1,
								   high.profit == 0 & labour.intensive == 1 ~ 0)
  )

triQc %>% sum_up(c(high.payroll.revenue, high.wages.revenue, high.profit, labour.intensive,
				   high.profit.labour, low.profit.labour, high.profit.capital, low.profit.capital))
#======================================================================================================================#
### Onsite: High Profit and Labour Intensive vs. High Profit Capital Intensive (high profit and Technology)
#======================================================================================================================#
### Onsite: Total releases intensity (high profit and labour intensive)
#======================================================================================================================#
sdid_total_releases_hpci <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "high.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_hpci, digits = 3, digits.stats = 3)
etable(sdid_total_releases_hpci, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_total_releases_hpci, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_total_releases_hpci, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Total Onsite Releases Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_hpli <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "high.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_hpli, digits = 3, digits.stats = 3)
iplot(sdid_total_releases_hpli, xlim = c(-3, 3), ylim = c(-0.35, 0.6), col = "blue",
	  main = "Total Onsite Releases Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (high profit and labour intensive)
#======================================================================================================================#
sdid_air_hpci <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "high.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_hpci, digits = 3, digits.stats = 3)
etable(sdid_air_hpci, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_air_hpci, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_air_hpci, xlim = c(-3, 3), ylim = c(-0.5, 0.65), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_hpli <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "high.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_hpli, digits = 3, digits.stats = 3)
iplot(sdid_air_hpli, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (high profit and labour intensive)
#======================================================================================================================#
sdid_point_air_hpci <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "high.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_hpci, digits = 3, digits.stats = 3)
etable(sdid_point_air_hpci, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_point_air_hpci, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_point_air_hpci, xlim = c(-3, 3), ylim = c(-0.3, 0.6), col = "blue",
	  main = "Total Point Air Emissions Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_hpli <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "high.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_hpli, digits = 3, digits.stats = 3)
iplot(sdid_point_air_hpli, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Fugitive air emissions intensity (high profit and labour intensive)
#======================================================================================================================#
sdid_fug_air_hpci <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "high.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_hpci, digits = 3, digits.stats = 3)
etable(sdid_fug_air_hpci, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_fug_air_hpci, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_fug_air_hpci, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_hpli <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "high.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_hpli, digits = 3, digits.stats = 3)
iplot(sdid_fug_air_hpli, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Water Discharge intensity (high profit and labour intensive)
#======================================================================================================================#
sdid_water_disc_hpci <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "high.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_hpci, digits = 3, digits.stats = 3)
etable(sdid_water_disc_hpci, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_water_disc_hpci, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_water_disc_hpci, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_hpli <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "high.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_hpli, digits = 3, digits.stats = 3)
iplot(sdid_water_disc_hpli, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Surface Water Discharge Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Land Releases intensity (high profit and labour intensive)
#======================================================================================================================#
sdid_land_releases_hpci <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "high.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_hpci, digits = 3, digits.stats = 3)
etable(sdid_land_releases_hpci, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases_hpci, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_land_releases_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_hpli <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "high.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_hpli, digits = 3, digits.stats = 3)
iplot(sdid_land_releases_hpli, xlim = c(-3, 3), ylim = c(-0.05, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Impoundment intensity (high profit and labour intensive)
#======================================================================================================================#
sdid_surface_impoundment_hpci <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "high.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_hpci, digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_hpci, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_hpci, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_hpci, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
	  main = "Total Surface Impoundment Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_hpli <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "high.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_hpli, digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_hpli, xlim = c(-3, 3), ylim = c(-0.03, 0.08), col = "blue",
	  main = "Total Surface Impoundment Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_highprofitcap.pdf", width =
  22,
	height = 10)
par(mfrow = c(3, 5))
iplot(sdid_total_releases_hpci, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Total Onsite Releases Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_total_releases_hpli, xlim = c(-3, 3), ylim = c(-0.35, 0.6), col = "blue",
	  main = "Total Onsite Releases Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_hpci, xlim = c(-3, 3), ylim = c(-0.5, 0.65), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_hpli, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_hpci, xlim = c(-3, 3), ylim = c(-0.3, 0.6), col = "blue",
	  main = "Total Point Air Emissions Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_hpli, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_hpci, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_hpli, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_hpci, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_hpli, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Surface Water Discharge Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_hpli, xlim = c(-3, 3), ylim = c(-0.05, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_hpci, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
	  main = "Total Surface Impoundment Intensity, HPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_hpli, xlim = c(-3, 3), ylim = c(-0.03, 0.08), col = "blue",
	  main = "Total Surface Impoundment Intensity, HPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Onsite: Low Profit and Capital Intensive vs. Low Profit Labour Intensive (low profit and production technology)
#======================================================================================================================#
### Onsite: Total releases intensity (low profit and capital intensive)
#======================================================================================================================#
sdid_total_releases_lpci <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "low.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_lpci, digits = 3, digits.stats = 3)
etable(sdid_total_releases_lpci, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_total_releases_lpci, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_total_releases_lpci, xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = "blue",
	  main = "Total Onsite Releases Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_total_releases_lpli <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  interact_var = "low.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_total_releases_lpli, digits = 3, digits.stats = 3)
iplot(sdid_total_releases_lpli, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total air emissions intensity (low profit and capital intensive)
#======================================================================================================================#
sdid_air_lpci <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "low.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_lpci, digits = 3, digits.stats = 3)
etable(sdid_air_lpci, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_air_lpci, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_air_lpci, xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_air_lpli <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  interact_var = "low.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_air_lpli, digits = 3, digits.stats = 3)
iplot(sdid_air_lpli, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total point air emissions intensity (low profit and capital intensive)
#======================================================================================================================#
sdid_point_air_lpci <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "low.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_lpci, digits = 3, digits.stats = 3)
etable(sdid_point_air_lpci, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_point_air_lpci, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_point_air_lpci, xlim = c(-3, 3), ylim = c(-0.5, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_point_air_lpli <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  interact_var = "low.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_point_air_lpli, digits = 3, digits.stats = 3)
iplot(sdid_point_air_lpli, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Fugitive air emissions intensity (low profit and capital intensive)
#======================================================================================================================#
sdid_fug_air_lpci <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "low.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_lpci, digits = 3, digits.stats = 3)
etable(sdid_fug_air_lpci, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_fug_air_lpci, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_fug_air_lpci, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_fug_air_lpli <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  interact_var = "low.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_fug_air_lpli, digits = 3, digits.stats = 3)
iplot(sdid_fug_air_lpli, xlim = c(-3, 3), ylim = c(-0.35, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Water Discharge intensity (low profit and capital intensive)
#======================================================================================================================#
sdid_water_disc_lpci <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "low.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_lpci, digits = 3, digits.stats = 3)
etable(sdid_water_disc_lpci, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_water_disc_lpci, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_water_disc_lpci, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_water_disc_lpli <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  interact_var = "low.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_water_disc_lpli, digits = 3, digits.stats = 3)
iplot(sdid_water_disc_lpli, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Land Releases intensity (low profit and capital intensive)
#======================================================================================================================#
sdid_land_releases_lpci <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "low.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_lpci, digits = 3, digits.stats = 3)
etable(sdid_land_releases_lpci, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases_lpci, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_land_releases_lpci, xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_land_releases_lpli <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  interact_var = "low.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_land_releases_lpli, digits = 3, digits.stats = 3)
iplot(sdid_land_releases_lpli, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Onsite: Total Surface Impoundment intensity (low profit and capital intensive)
#======================================================================================================================#
sdid_surface_impoundment_lpci <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "low.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_lpci, digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_lpci, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment_lpci, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_lpci, xlim = c(-3, 3), ylim = c(-0.02, 0.01), col = "blue",
	  main = "Total Surface Impoundment Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_surface_impoundment_lpli <- sdid_releases_heter_0(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  interact_var = "low.profit.capital",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_surface_impoundment_lpli, digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment_lpli, xlim = c(-3, 3), ylim = c(-0.02, 0.04), col = "blue",
	  main = "Total Surface Impoundment Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_onsite_releases_int_lowprofitcap.pdf", width = 22,
	height = 10)
par(mfrow = c(3, 5))
iplot(sdid_total_releases_lpci, xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = "blue",
	  main = "Total Onsite Releases Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_total_releases_lpli, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Onsite Releases Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_lpci, xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_air_lpli, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Onsite Air Emissions Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_lpci, xlim = c(-3, 3), ylim = c(-0.5, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_point_air_lpli, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
	  main = "Total Point Air Emissions Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_lpci, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_fug_air_lpli, xlim = c(-3, 3), ylim = c(-0.35, 0.25), col = "blue",
	  main = "Total Fugitive Air Emissions Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_lpci, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_water_disc_lpli, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Surface Water Discharge Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_lpci, xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_lpli, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
	  main = "Total Land Releases Intensity, LPLI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_lpci, xlim = c(-3, 3), ylim = c(-0.02, 0.01), col = "blue",
	  main = "Total Surface Impoundment Intensity, LPCI", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment_lpli, xlim = c(-3, 3), ylim = c(-0.02, 0.04), col = "blue",
	  main = "Total Surface Impoundment Intensity, LPLI", xlab = "relative year",
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