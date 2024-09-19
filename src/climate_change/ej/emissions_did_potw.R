#======================================================================================================================#
library(tidyverse)
library(statar)
library(fixest)
library(did)
library(car)
#======================================================================================================================#
## Working Directory
#======================================================================================================================#
setwd(dir = "your_path")
source(file = "your_path/functions.R", echo = T)
#======================================================================================================================#
### Loading Data
#======================================================================================================================#
file <- "your_path/triQc_potws_econj.rds"
triQc <- read_rds(file = file)
#======================================================================================================================#
### Labour Cost---Wage per hr, weekly wages, and total wages
#======================================================================================================================#
table(triQc$facility.state, triQc$ch.year)
n_distinct(triQc$chemical.name)
sort(unique(triQc$chemical.name))
sort(unique(triQc$potw.zipcode))
sort(unique(triQc$facility.state.id))
sort(unique(triQc$year))
sort(unique(triQc$rel.year))
#======================================================================================================================#
### POTW: Total releases
#======================================================================================================================#
sum_up(triQc, c(total.potw.releases.offsite, potw.releases.underground.other.offsite,
				potw.releases.underground.Iwells.offsite,
				potw.treatment.offsite, total.potw.management.offsite))

sdid_releases <- dynamic_sdid_releases_potws(
  data = triQc,
  depvar = "l.total.potw.releases.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_releases, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_releases, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_releases, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Total POTW Releases Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### POTW: Total underground releases
#======================================================================================================================#
sdid_underground_releases <- dynamic_sdid_releases_potws(
  data = triQc,
  depvar = "l.potw.releases.underground.Iwells.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_underground_releases, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_underground_releases, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_underground_releases, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Total POTW Underground Releases Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### POTW: Total underground releases, other
#======================================================================================================================#
sdid_underground_releases_other <- dynamic_sdid_releases_potws(
  data = triQc,
  depvar = "l.potw.releases.underground.other.offsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_underground_releases_other, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_underground_releases_other, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_underground_releases_other, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Total POTW Underground Releases Intensity, other", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### POTW: Treatment Offsite
#======================================================================================================================#
sdid_treatment <- dynamic_sdid_releases_potws(
  data = triQc,
  depvar = "l.potw.treatment.offsite",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_treatment, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_treatment, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_treatment, xlim = c(-3, 3), ylim = c(-0.8, 0.8), col = "blue",
	  main = "Total POTW Treatment Offsite", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### POTW: Total POTW Waste Management
#======================================================================================================================#
sdid_potw_waste_mgt <- dynamic_sdid_releases_potws(
  data = triQc,
  depvar = "l.total.potw.management.offsite",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  fes = tri_fes()
)
etable(sdid_potw_waste_mgt, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_potw_waste_mgt, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_potw_waste_mgt, xlim = c(-3, 3), ylim = c(-0.8, 0.8), col = "blue",
	  main = "Total POTW Waste Management", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_releases_potws.pdf", width = 12, height = 7)
par(mfrow = c(2, 3))
iplot(sdid_releases, xlim = c(-3, 3), ylim = c(-0.4, 0.5), col = "blue",
	  main = "Total POTW Releases Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_underground_releases, xlim = c(-3, 3), ylim = c(-0.5, 0.3), col = "blue",
	  main = "Total POTW Underground Releases Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_underground_releases_other, xlim = c(-3, 3), ylim = c(-0.4, 0.5), col = "blue",
	  main = "Total POTW Underground Releases Intensity, Other", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_treatment, xlim = c(-3, 3), ylim = c(-0.8, 0.8), col = "blue",
	  main = "Total Offsite Treatment, POTW", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_potw_waste_mgt, xlim = c(-3, 3), ylim = c(-0.8, 0.8), col = "blue",
	  main = "Total Offsite Waste Management, POTW", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#