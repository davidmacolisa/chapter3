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
source(file = "./Thesis/chapter3/src/climate_change/codes/functions.R", echo = T)
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
sort(unique(triQc$year))
sort(unique(triQc$rel.year))
#======================================================================================================================#
# Variables Selection for Journal
#======================================================================================================================#
# triQc <- triQc %>%
#   select(
# 	c(
# 	  # Dependent Variables
# 	  year:unit.of.measure, treated:dist.to.border, e.treated, rel.year, post,
#
# 	  total.releases.offsite.intensity, total.releases.unknown.offsite.intensity,
# 	  total.releases.wastebroker.offsite.intensity, total.releases.other.mgt.offsite.intensity,
# 	  total.releases.metalsolidify.offsite.intensity, total.releases.storage.offsite.intensity,
# 	  total.wastewater.releases.offsite.intensity, total.land.releases.offsite.intensity,
# 	  total.land.releases.other.offsite.intensity, total.surface.impoundment.offsite.intensity,
# 	  total.releases.toland.treatment.offsite.intensity, total.landfills.offsite.intensity,
# 	  total.underground.injection.offsite.intensity,
#
# 	  # Independent Variables
# 	  gdppc.1, annual.avg.estabs.1, cpi.1, federal.facility, produced.chem.facility,
# 	  imported.chem.facility, chemical.formulation.component, chemical.article.component,
# 	  chemical.manufacturing.aid, chemical.ancilliary.use, production.ratio.activity.index,
# 	  maxnum.chem.onsite, clean.air.act.chems, hap.chems, pbt.chems,
#
# 	  # Fixed effects
# 	  facility.id.fe, chemical.id.fe, fips.code.fe, border.county.fe, border.county.year,
# 	  border.county.year.fe, chemical.year.fe, border.state.fe, border.state.year.fe
# 	)
#   )
# write_rds(x = triQc, file = "./Thesis/chapter3/src/climate_change/data/triQ_offsite_econj.rds", compress = "gz")
#======================================================================================================================#
### Offsite: Total releases
#======================================================================================================================#
sdid_releases <- dynamic_sdid_releases_offsite(
  data = triQc,
  depvar = "l.total.releases.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_releases, digits = 3, digits.stats = 3)
etable(sdid_releases, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_releases, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_releases, xlim = c(-3, 3), ylim = c(-0.1, 0.5), col = "blue",
	  main = "Toal Offsite Releases Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total releases, unknown
#======================================================================================================================#
sdid_releases_unknown <- dynamic_sdid_releases_offsite(
  data = triQc,
  depvar = "l.total.releases.unknown.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_releases_unknown, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_releases_unknown, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_releases_unknown, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Offsite Releases Intensity, Unknown", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total releases, waste broker
#======================================================================================================================#
sdid_releases_wastebroker <- dynamic_sdid_releases_offsite(
  data = triQc,
  depvar = "l.total.releases.wastebroker.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_releases_wastebroker, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_releases_wastebroker, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_releases_wastebroker, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Offsite Releases Intensity, Wastebroker", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total releases, others
#======================================================================================================================#
sdid_releases_others <- dynamic_sdid_releases_offsite(
  data = triQc,
  depvar = "l.total.releases.other.mgt.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_releases_others, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_releases_others, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_releases_others, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Offsite Releases Intensity, Others", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total releases, metal solidification
#======================================================================================================================#
sdid_releases_metalsolidify <- dynamic_sdid_releases_offsite(
  data = triQc,
  depvar = "l.total.releases.metalsolidify.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_releases_metalsolidify, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_releases_metalsolidify, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_releases_metalsolidify, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Offsite Releases Intensity, Metal Solidification", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total releases, storage
#======================================================================================================================#
sdid_releases_storage <- dynamic_sdid_releases_offsite(
  data = triQc,
  depvar = "l.total.releases.storage.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_releases_storage, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_releases_storage, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_releases_storage, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
	  main = "Total Offsite Releases Intensity, Storage", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_releases_offsite.pdf", width = 12, height = 7)
par(mfrow = c(2, 3))
iplot(sdid_releases, xlim = c(-3, 3), ylim = c(-0.2, 0.6), col = "blue",
	  main = "Total Offsite Releases Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_releases_unknown, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Offsite Releases Intensity, Unknown", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_releases_wastebroker, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Offsite Releases Intensity, Waste Broker", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_releases_others, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Offsite Releases Intensity, Others", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_releases_metalsolidify, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Total Offsite Releases Intensity, Metal Solidification", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_releases_storage, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
	  main = "Total Offsite Releases Intensity, Storage", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Offsite: Total land releases
#======================================================================================================================#
sdid_land_releases <- dynamic_sdid_releases_offsite(
  data = triQc,
  depvar = "l.total.land.releases.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_land_releases, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_land_releases, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Total Land Releases Intensity, Offsite", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total land releases, others
#======================================================================================================================#
sdid_land_releases_others <- dynamic_sdid_releases_offsite(
  data = triQc,
  depvar = "l.total.land.releases.other.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_land_releases_others, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases_others, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_land_releases_others, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Offsite Land Releases Intensity, Others", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total waste water releases
#======================================================================================================================#
sdid_waste_water_releases <- dynamic_sdid_releases_offsite(
  data = triQc,
  depvar = "l.total.wastewater.releases.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_waste_water_releases, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_waste_water_releases, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_waste_water_releases, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Waste Water Releases Intensity, Offsite", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total surface impoundment
#======================================================================================================================#
sdid_surface_impoundment <- dynamic_sdid_releases_offsite(
  data = triQc,
  depvar = "l.total.surface.impoundment.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_surface_impoundment, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_surface_impoundment, xlim = c(-3, 3), ylim = c(-0.01, 0.02), col = "blue",
	  main = "Total Surface Impoundment Intensity, Offsite", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total toland treatment
#======================================================================================================================#
sdid_toland_treatment <- dynamic_sdid_releases_offsite(
  data = triQc,
  depvar = "l.total.releases.toland.treatment.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_toland_treatment, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_toland_treatment, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_toland_treatment, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
	  main = "Total ToLand Treatment Intensity, Offsite", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total landfills
#======================================================================================================================#
sdid_landfills <- dynamic_sdid_releases_offsite(
  data = triQc,
  depvar = "l.total.landfills.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_landfills, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_landfills, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_landfills, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Landfills Intensity, Offsite", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Offsite: Total Underground injection
#======================================================================================================================#
sdid_underground_injection <- dynamic_sdid_releases_offsite(
  data = triQc,
  depvar = "l.total.underground.injection.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_underground_injection, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_underground_injection, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_underground_injection, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
	  main = "Total Underground Injection Intensity, Offsite", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_land_releases_offsite.pdf", width = 12, height
  = 10)
par(mfrow = c(3, 3))
iplot(sdid_land_releases, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Total Land Releases Intensity, Offsite", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_land_releases_others, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Offsite Land Releases Intensity, Others", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_waste_water_releases, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Waste Water Releases Intensity, Offsite", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_surface_impoundment, xlim = c(-3, 3), ylim = c(-0.01, 0.02), col = "blue",
	  main = "Total Surface Impoundment Intensity, Offsite", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_toland_treatment, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
	  main = "Total ToLand Treatment Intensity, Offsite", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_landfills, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Landfills Intensity, Offsite", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_underground_injection, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
	  main = "Total Underground Injection Intensity, Offsite", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#