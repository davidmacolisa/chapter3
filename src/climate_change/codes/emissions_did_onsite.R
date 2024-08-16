#======================================================================================================================#
### PhD Chapter 3
### Unforeseen Minimum Wage Consequences
### 30 October 2023
### Use Regression Discontinuity Analysis
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
### Loading Functions and Data
#======================================================================================================================#
source(file = "./Thesis/chapter3/src/climate_change/codes/functions.R", echo = T)
file <- "./Data_PhD/US/BLS/onsite/triQc_on.rds"
triQc <- read_rds(file = file)

sort(unique(triQc$year))
sort(unique(triQc$rel.year))
#======================================================================================================================#
### Onsite: Total releases intensity
#======================================================================================================================#
did_total_releases <- did_releases(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  ATT = "e.treated",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(did_total_releases, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.total.releases.onsite.intensity",
  G = "fips.code",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
did_total_releases <- dynamic_did_releases(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  relative_year = "rel.year",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(did_total_releases, digits = 3, digits.stats = 3)
iplot(did_total_releases, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Total Onsite Releases Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_total_releases)[grep(pattern = "rel.year", names(coef(did_total_releases)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(did_total_releases, paste0(names(pre.treat.coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_total_releases <- sdid_releases(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(sdid_total_releases, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_total_releases, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_total_releases, digits = 3, digits.stats = 3)

sdid_total_releases <- dynamic_sdid_releases(
  data = triQc,
  depvar = "l.total.releases.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(sdid_total_releases, digits.stats = 3, digits = 3)
iplot(list(sdid_total_releases, did_total_releases),
	  xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = c("blue", "pink"),
	  main = "Total Onsite Releases Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
# Add a legend to the plot
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.119*** (0.040)", "TWFE ATT: 0.133*** (0.045)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_releases_onsite_int.pdf", width = 8, height = 5.5)
iplot(
  list(sdid_total_releases, did_total_releases),
  xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = c("blue", "pink"),
  main = "Total Onsite Releases Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
# Add a legend to the plot
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.119*** (0.040)", "TWFE ATT: 0.133*** (0.045)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
dev.off()
#======================================================================================================================#
### Onsite: Total air emissions intensity
#======================================================================================================================#
did_air <- did_releases(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  ATT = "e.treated",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(did_air, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.total.air.emissions.onsite.intensity",
  G = "fips.code",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
did_air <- dynamic_did_releases(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  relative_year = "rel.year",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(did_air, digits = 3, digits.stats = 3)
iplot(did_air, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
	  main = "Total Onsite Air Emissions Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_air)[grep(pattern = "rel.year", names(coef(did_air)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(did_air, paste0(names(pre.treat.coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_air <- sdid_releases(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(sdid_air, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_air, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_air, digits = 3, digits.stats = 3)

sdid_air <- dynamic_sdid_releases(
  data = triQc,
  depvar = "l.total.air.emissions.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(sdid_air, digits.stats = 3, digits = 3)
iplot(
  list(sdid_air, did_air),
  xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = c("blue", "pink"),
  main = "Total Onsite Air Emissions Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.102*** (0.032)", "TWFE ATT: 0.082** (0.034)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Onsite: Total point air emissions intensity
#======================================================================================================================#
did_point_air <- did_releases(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  ATT = "e.treated",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(did_point_air, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.total.point.air.emissions.onsite.intensity",
  G = "fips.code",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
did_point_air <- dynamic_did_releases(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  relative_year = "rel.year",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(did_point_air, digits = 3, digits.stats = 3)
iplot(did_point_air, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Onsite Point Air Emissions Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_point_air)[grep(pattern = "rel.year", names(coef(did_point_air)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(did_point_air, paste0(names(pre.treat.coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_point_air <- sdid_releases(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(sdid_point_air, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_point_air, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_point_air, digits = 3, digits.stats = 3)

sdid_point_air <- dynamic_sdid_releases(
  data = triQc,
  depvar = "l.total.point.air.emissions.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(sdid_point_air, digits.stats = 3, digits = 3)
iplot(
  list(sdid_point_air, did_point_air),
  xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = c("blue", "pink"),
  main = "Total Onsite Point Air Emissions Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.049* (0.026)", "TWFE ATT: 0.034 (0.028)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity
#======================================================================================================================#
did_fug_air <- did_releases(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  ATT = "e.treated",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(did_fug_air, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.total.fug.air.emissions.onsite.intensity",
  G = "fips.code",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
did_fug_air <- dynamic_did_releases(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  relative_year = "rel.year",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(did_fug_air, digits = 3, digits.stats = 3)
iplot(did_fug_air, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Onsite Fugitive Air Emissions Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_fug_air)[grep(pattern = "rel.year", names(coef(did_fug_air)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(did_fug_air, paste0(names(pre.treat.coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_fug_air <- sdid_releases(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(sdid_fug_air, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_fug_air, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_fug_air, digits = 3, digits.stats = 3)

sdid_fug_air <- dynamic_sdid_releases(
  data = triQc,
  depvar = "l.total.fug.air.emissions.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(sdid_fug_air, digits.stats = 3, digits = 3)
iplot(
  list(sdid_fug_air, did_fug_air),
  xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = c("blue", "pink"),
  main = "Total Onsite Fugitive Air Emissions Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.072** (0.029)", "TWFE ATT: 0.055** (0.026)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_onsite_air_emissions_int.pdf", width = 16, height = 4)
par(mfrow = c(1, 3))
iplot(
  list(sdid_air, did_air),
  xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = c("blue", "pink"),
  main = "Total Onsite Air Emissions Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.102*** (0.032)", "TWFE ATT: 0.082** (0.034)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_point_air, did_point_air),
  xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = c("blue", "pink"),
  main = "Total Onsite Point Air Emissions Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.049* (0.026)", "TWFE ATT: 0.034 (0.028)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_fug_air, did_fug_air),
  xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = c("blue", "pink"),
  main = "Total Onsite Fugitive Air Emissions Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.072** (0.029)", "TWFE ATT: 0.055** (0.026)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
dev.off()
#======================================================================================================================#
### Onsite: Total surface water discharge intensity
#======================================================================================================================#
did_water_disc <- did_releases(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  ATT = "e.treated",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(did_water_disc, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.total.surface.water.discharge.onsite.intensity",
  G = "fips.code",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
did_water_disc <- dynamic_did_releases(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  relative_year = "rel.year",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(did_water_disc, digits = 3, digits.stats = 3)
iplot(did_water_disc, xlim = c(-3, 3), ylim = c(-0.25, 0.25), col = "blue",
	  main = "Total Onsite Surface Water Discharge Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_water_disc)[grep(pattern = "rel.year", names(coef(did_water_disc)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(did_water_disc, paste0(names(pre.treat.coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_water_disc <- sdid_releases(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(sdid_water_disc, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_water_disc, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_water_disc, digits = 3, digits.stats = 3)

did_water_disc <- dynamic_sdid_releases(
  data = triQc,
  depvar = "l.total.surface.water.discharge.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(sdid_water_disc, digits.stats = 3, digits = 3)
iplot(
  list(sdid_water_disc, did_water_disc),
  xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = c("blue", "pink"),
  main = "Total Onsite Surface Water Discharge Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.014 (0.030)", "TWFE ATT: 0.039 (0.036)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Onsite: Number of receiving streams
#======================================================================================================================#
did_receiving_streams <- did_releases(
  data = triQc,
  depvar = "l.total.num.receiving.streams.onsite",
  ATT = "e.treated",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(did_receiving_streams, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.total.num.receiving.streams.onsite",
  G = "fips.code",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
did_receiving_streams <- dynamic_did_releases(
  data = triQc,
  depvar = "l.total.num.receiving.streams.onsite",
  relative_year = "rel.year",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(did_receiving_streams, digits = 3, digits.stats = 3)
iplot(did_receiving_streams, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
	  main = "Total Onsite Number of Receiving Streams", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_receiving_streams)[grep(pattern = "rel.year", names(coef(did_receiving_streams)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(did_receiving_streams, paste0(names(pre.treat.coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_receiving_streams <- sdid_releases(
  data = triQc,
  depvar = "l.total.num.receiving.streams.onsite",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(sdid_receiving_streams, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_receiving_streams, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_receiving_streams, digits = 3, digits.stats = 3)

sdid_receiving_streams <- dynamic_did_releases(
  data = triQc,
  depvar = "l.total.num.receiving.streams.onsite",
  relative_year = "rel.year",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(sdid_receiving_streams, digits.stats = 3, digits = 3)
iplot(
  list(sdid_receiving_streams, did_receiving_streams),
  xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = c("blue", "pink"),
  main = "Total Number of Receiving Streams (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.018*** (0.006)", "TWFE ATT: -0.008 (0.006)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_onsite_water_discharge_int.pdf", width = 15, height = 5)
par(mfrow = c(1, 2))
iplot(
  list(sdid_water_disc, did_water_disc),
  xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = c("blue", "pink"),
  main = "Total Onsite Surface Water Discharge Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.014 (0.030)", "TWFE ATT: 0.039 (0.036)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_receiving_streams, did_receiving_streams),
  xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = c("blue", "pink"),
  main = "Total Number of Receiving Streams (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.018*** (0.006)", "TWFE ATT: -0.008 (0.006)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
dev.off()
#======================================================================================================================#
### Onsite: Total land releases intensity
#======================================================================================================================#
did_land_releases <- did_releases(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  ATT = "e.treated",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(did_land_releases, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.total.land.releases.onsite.intensity",
  G = "fips.code",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
did_land_releases <- dynamic_did_releases(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  relative_year = "rel.year",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(did_land_releases, digits = 3, digits.stats = 3)
iplot(did_land_releases, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
	  main = "Total Onsite Land Releases Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases)[grep(pattern = "rel.year", names(coef(did_land_releases)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(did_land_releases, paste0(names(pre.treat.coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_land_releases <- sdid_releases(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(sdid_land_releases, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_land_releases, digits = 3, digits.stats = 3)

sdid_land_releases <- dynamic_sdid_releases(
  data = triQc,
  depvar = "l.total.land.releases.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(sdid_land_releases, digits.stats = 3, digits = 3)
iplot(
  list(sdid_land_releases, did_land_releases),
  xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = c("blue", "pink"),
  main = "Total Onsite Land Releases Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.011 (0.016)", "TWFE ATT: 0.010 (0.010)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Onsite: Total underground injection intensity
#======================================================================================================================#
did_undground_inject <- did_releases(
  data = triQc,
  depvar = "l.total.underground.injection.onsite.intensity",
  ATT = "e.treated",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(did_undground_inject, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.total.underground.injection.onsite.intensity",
  G = "fips.code",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
did_undground_inject <- dynamic_did_releases(
  data = triQc,
  depvar = "l.total.underground.injection.onsite.intensity",
  relative_year = "rel.year",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(did_undground_inject, digits = 3, digits.stats = 3)
iplot(did_undground_inject, xlim = c(-3, 3), ylim = c(-0.009, 0.009), col = "blue",
	  main = "Total Onsite Underground Injection Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_undground_inject)[grep(pattern = "rel.year", names(coef(did_undground_inject)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(did_undground_inject, paste0(names(pre.treat.coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_undground_inject <- sdid_releases(
  data = triQc,
  depvar = "l.total.underground.injection.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(sdid_undground_inject, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_undground_inject, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_undground_inject, digits = 3, digits.stats = 3)

sdid_undground_inject <- dynamic_sdid_releases(
  data = triQc,
  depvar = "l.total.underground.injection.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(sdid_undground_inject, digits.stats = 3, digits = 3)
iplot(
  list(sdid_undground_inject, did_undground_inject),
  xlim = c(-3, 3), ylim = c(-0.005, 0.005), col = c("blue", "pink"),
  main = "Total Onsite Underground Injection Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.000 (0.000)", "TWFE ATT: -0.000 (0.000)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Onsite: Total landfills intensity
#======================================================================================================================#
did_landfills <- did_releases(
  data = triQc,
  depvar = "l.total.landfills.onsite.intensity",
  ATT = "e.treated",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(did_landfills, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.total.landfills.onsite.intensity",
  G = "fips.code",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
did_landfills <- dynamic_did_releases(
  data = triQc,
  depvar = "l.total.landfills.onsite.intensity",
  relative_year = "rel.year",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(did_landfills, digits = 3, digits.stats = 3)
iplot(did_landfills, xlim = c(-3, 3), ylim = c(-0.03, 0.03), col = "blue",
	  main = "Total Onsite Landfills Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_landfills)[grep(pattern = "rel.year", names(coef(did_landfills)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(did_landfills, paste0(names(pre.treat.coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_landfills <- sdid_releases(
  data = triQc,
  depvar = "l.total.landfills.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(sdid_landfills, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_landfills, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_landfills, digits = 3, digits.stats = 3)

sdid_landfills <- dynamic_sdid_releases(
  data = triQc,
  depvar = "l.total.landfills.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(sdid_landfills, digits.stats = 3, digits = 3)
iplot(
  list(sdid_landfills, did_landfills),
  xlim = c(-3, 3), ylim = c(-0.03, 0.03), col = c("blue", "pink"),
  main = "Total Onsite Landfills Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.000 (0.003)", "TWFE ATT: -0.004 (0.004)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Onsite: Total releases to land treatment intensity
#======================================================================================================================#
did_release_toland <- did_releases(
  data = triQc,
  depvar = "l.total.releases.toland.treatment.onsite.intensity",
  ATT = "e.treated",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(did_release_toland, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.total.releases.toland.treatment.onsite.intensity",
  G = "fips.code",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
did_release_toland <- dynamic_did_releases(
  data = triQc,
  depvar = "l.total.releases.toland.treatment.onsite.intensity",
  relative_year = "rel.year",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(did_release_toland, digits = 3, digits.stats = 3)
iplot(did_release_toland, xlim = c(-3, 3), ylim = c(-0.05, 0.05), col = "blue",
	  main = "Total Onsite Releases to Land Treatment Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_release_toland)[grep(pattern = "rel.year", names(coef(did_release_toland)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(did_release_toland, paste0(names(pre.treat.coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_release_toland <- sdid_releases(
  data = triQc,
  depvar = "l.total.releases.toland.treatment.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(sdid_release_toland, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_release_toland, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_release_toland, digits = 3, digits.stats = 3)

sdid_release_toland <- dynamic_sdid_releases(
  data = triQc,
  depvar = "l.total.releases.toland.treatment.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(sdid_release_toland, digits.stats = 3, digits = 3)
iplot(
  list(sdid_release_toland, did_release_toland),
  xlim = c(-3, 3), ylim = c(-0.05, 0.05), col = c("blue", "pink"),
  main = "Total Onsite Releases to Land Treatment Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.007 (0.012)", "TWFE ATT: -0.002 (0.006)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Onsite: Total surface impoundment intensity
#======================================================================================================================#
did_surface_impoundment <- did_releases(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  ATT = "e.treated",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(did_surface_impoundment, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.total.surface.impoundment.onsite.intensity",
  G = "fips.code",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
did_surface_impoundment <- dynamic_did_releases(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  relative_year = "rel.year",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(did_surface_impoundment, digits = 3, digits.stats = 3)
iplot(did_surface_impoundment, xlim = c(-3, 3), ylim = c(-0.02, 0.08), col = "blue",
	  main = "Total Onsite Surface Impoundment Intensity", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_surface_impoundment)[grep(pattern = "rel.year", names(coef(did_surface_impoundment)))]
pre.treat.coef <- pre.treat.coef[4:5]
linearHypothesis(did_surface_impoundment, paste0(names(pre.treat.coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_surface_impoundment <- sdid_releases(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(sdid_surface_impoundment, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_surface_impoundment, digits = 3, digits.stats = 3)

sdid_surface_impoundment <- dynamic_sdid_releases(
  data = triQc,
  depvar = "l.total.surface.impoundment.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(sdid_surface_impoundment, digits.stats = 3, digits = 3)
iplot(
  list(sdid_surface_impoundment, did_surface_impoundment),
  xlim = c(-3, 3), ylim = c(-0.015, 0.05), col = c("blue", "pink"),
  main = "Total Onsite Surface Impoundment Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.011* (0.006)", "TWFE ATT: 0.010** (0.005)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Onsite: Total land releases others intensity
#======================================================================================================================#
did_land_releases_others <- did_releases(
  data = triQc,
  depvar = "l.total.land.releases.other.onsite.intensity",
  ATT = "e.treated",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(did_land_releases_others, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.total.land.releases.other.onsite.intensity",
  G = "fips.code",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
did_land_releases_others <- dynamic_did_releases(
  data = triQc,
  depvar = "l.total.land.releases.other.onsite.intensity",
  relative_year = "rel.year",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(did_land_releases_others, digits = 3, digits.stats = 3)
iplot(did_land_releases_others, xlim = c(-3, 3), ylim = c(-0.1, 0.1), col = "blue",
	  main = "Toal Onsite Land Releases Intensity, Others", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(did_land_releases_others)[grep(pattern = "rel.year", names(coef(did_land_releases_others)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(did_land_releases_others, paste0(names(pre.treat.coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_land_releases_others <- sdid_releases(
  data = triQc,
  depvar = "l.total.land.releases.other.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  did_tri_fes = did_tri_fes()
)
etable(sdid_land_releases_others, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_land_releases_others, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_land_releases_others, digits = 3, digits.stats = 3)

sdid_land_releases_others <- dynamic_sdid_releases(
  data = triQc,
  depvar = "l.total.land.releases.other.onsite.intensity",
  ATT = "sunab(ch.year, year)",
  cluster = ~c(chemical.id, naics.code, facility.state),
  tri_fes = tri_fes
)
etable(sdid_land_releases_others, digits.stats = 3, digits = 3)
iplot(
  list(sdid_land_releases_others, did_land_releases_others),
  xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = c("blue", "pink"),
  main = "Total Onsite Land Releases Intensity, Others (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.029** (0.013)", "TWFE ATT: 0.006 (0.007)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_total_land_releases_onsite_int.pdf", width = 16,
	height = 7)
par(mfrow = c(2, 3))
iplot(
  list(sdid_land_releases, did_land_releases),
  xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = c("blue", "pink"),
  main = "Total Onsite Land Releases Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.011 (0.016)", "TWFE ATT: 0.010 (0.010)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_undground_inject, did_undground_inject),
  xlim = c(-3, 3), ylim = c(-0.005, 0.005), col = c("blue", "pink"),
  main = "Total Onsite Underground Injection Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.000 (0.000)", "TWFE ATT: -0.000 (0.000)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_landfills, did_landfills),
  xlim = c(-3, 3), ylim = c(-0.03, 0.03), col = c("blue", "pink"),
  main = "Total Onsite Landfills Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.000 (0.003)", "TWFE ATT: -0.004 (0.004)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_release_toland, did_release_toland),
  xlim = c(-3, 3), ylim = c(-0.05, 0.05), col = c("blue", "pink"),
  main = "Total Onsite Releases to Land Treatment Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.007 (0.012)", "TWFE ATT: -0.002 (0.006)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_surface_impoundment, did_surface_impoundment),
  xlim = c(-3, 3), ylim = c(-0.015, 0.05), col = c("blue", "pink"),
  main = "Total Onsite Surface Impoundment Intensity (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.011* (0.006)", "TWFE ATT: 0.010** (0.005)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_land_releases_others, did_land_releases_others),
  xlim = c(-3, 3), ylim = c(-0.15, 0.05), col = c("blue", "pink"),
  main = "Total Onsite Land Releases Intensity, Others (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.029** (0.013)", "TWFE ATT: 0.006 (0.007)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
dev.off()
#======================================================================================================================#