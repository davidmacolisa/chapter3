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
library(car)
library(DIDmultiplegt)
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
### Not yet treated group
#======================================================================================================================#
triQc_nyt <- triQc %>% filter(ch.year != Inf)
table(triQc_nyt$year, triQc_nyt$ch.year)
#======================================================================================================================#
### Controls
#======================================================================================================================#
controls <- ~
  # gdppc.1 +
  #   annual.avg.estabs.1 +
  # population.1 +
  # cpi.1 +
  # entire.facility +
  # private.facility +
  # federal.facility +
  # year +
  # fips.code +
  # border.county.fe +
  # facility.state.fe +
  border.county.year.fe
#======================================================================================================================#
### Onsite: Total releases intensity
#======================================================================================================================#
start_time <- Sys.time()
sdid_wages <- att_gt(
  yname = "wage.perhr",
  tname = "year",
  idname = "fips.code",
  gname = "ch.year",
  # xformla = controls,
  panel = T,
  allow_unbalanced_panel = T,
  control_group = "notyettreated",
  clustervars = "facility.state.fe",
  est_method = "reg",
  pl = T,
  base_period = "universal",
  data = triQc_nyt
)
end_time <- Sys.time()
end_time - start_time

summary(sdid_wages)
sdid_wages_es <- aggte(
  sdid_wages,
  type = "dynamic",
  bstrap = T,
  cband = T,
  # min_e = -4,
  # max_e = 3
)
summary(sdid_wages_es)
ggdid(sdid_wages_es)

summary(aggte(sdid_wages, type = 'group'))
sdid_wages_grp <- ggdid(aggte(sdid_wages, type = 'group'))


did2s_wages_perhr <- did2s(
  data = triQc,
  yname = "l.wage.perhr",
  first_stage = ~
    gdppc.1 +
      annual.avg.estabs.1 +
      population.1 +
      cpi.1 +
      entire.facility +
      private.facility +
      federal.facility
      |
      year +
        # fips.code +
        border.county.fe
  # border.county.year.fe
  ,
  # second_stage = ~i(factor_var = e.treated, ref = 0),
  second_stage = ~i(rel.year, ref = c(-1, Inf)),
  treatment = "e.treated",
  cluster_var = c("facility.state"),
  # panel = T
)
etable(did2s_wages_perhr, digits.stats = 4, digits = 4)
fixest::iplot(did2s_wages_perhr, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Hourly wage", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)