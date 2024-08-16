#======================================================================================================================#
### PhD Chapter 3
### Indirect Consequences of a Raising Minimum Wage
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
### Loading Data
#======================================================================================================================#
source(file = "./Thesis/chapter3/src/climate_change/codes/functions.R", echo = T)
file <- "./Data_PhD/US/BLS/onsite/triQc_on.rds"
triQc <- read_rds(file = file)
#======================================================================================================================#
### Labour Cost---Wage per hour, per worker, material cost, profits, and outputs.
#======================================================================================================================#
table(triQc$facility.state, triQc$ch.year)
n_distinct(triQc$chemical.name)
sort(unique(triQc$chemical.name))
sort(unique(triQc$year))
sort(unique(triQc$rel.year))
#======================================================================================================================#
### Treatment Selection
#======================================================================================================================#
treat_sel <- feols(
  e.treated ~ sw0(
	gdp.1 +
	  gdppc.1 +
	  pinc.1 +
	  annual.avg.estabs.1 +
	  cpi.1 +
	  entire.facility +
	  private.facility +
	  federal.facility
  )
	|
	csw(,
	  year,
	  fips.code.fe,
	  border.county.fe,
	  border.county.year.fe
	)
  ,
  data = triQc,
  cluster = ~facility.state,
)
etable(treat_sel, digits = 3, digits.stats = 3)
#======================================================================================================================#
### Wage per hour
#======================================================================================================================#
reg_wagephr <- did_preliminary(
  data = triQc,
  depvar = "wage.perhr",
  ATT = "e.treated",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(reg_wagephr, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.wage.perhr",
  G = "fips.code.fe",
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
reg_wagephr <- dynamic_did_preliminary(
  data = triQc,
  depvar = "l.wage.perhr",
  relative_year = "rel.year",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(reg_wagephr, digits = 3, digits.stats = 3)
iplot(reg_wagephr, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
	  main = "Hourly wage", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_wagephr)[grep(pattern = "rel.year", names(coef(reg_wagephr)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_wagephr, paste0(names(pre_treat_coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_wages <- sdid_preliminary(
  data = triQc,
  depvar = "wage.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(sdid_wages, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_wages, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_wages, digits.stats = 3, digits = 3)

sdid_wages <- dynamic_sdid_preliminary(
  data = triQc,
  depvar = "l.wage.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_wages, digits.stats = 3, digits = 3)
iplot(list(sdid_wages, reg_wagephr),
	  xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = c("blue", "pink"),
	  main = "Hourly wage (log)", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: $0.889* (0.452)", "TWFE ATT: $0.606* (0.325)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Labour cost: Industry Pay---Total payroll
#======================================================================================================================#
reg_pay <- did_preliminary(
  data = triQc,
  depvar = "pay",
  ATT = "e.treated",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(reg_pay, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "pay",
  G = "fips.code.fe",
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
reg_pay <- dynamic_did_preliminary(
  data = triQc,
  depvar = "l.pay",
  relative_year = "rel.year",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(reg_pay, digits = 3, digits.stats = 3)
iplot(reg_pay, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Total Payroll", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_pay)[grep(pattern = "rel.year", names(coef(reg_pay)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_pay, paste0(names(pre_treat_coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_pay <- sdid_preliminary(
  data = triQc,
  depvar = "l.pay",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(sdid_pay, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_pay, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_pay, digits.stats = 3, digits = 3)

sdid_pay <- dynamic_sdid_preliminary(
  data = triQc,
  depvar = "l.pay",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_pay, digits.stats = 3, digits = 3)
iplot(
  list(sdid_pay, reg_pay),
  xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = c("blue", "pink"),
  main = "Total Payroll (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.043* (0.025)", "TWFE ATT: 0.035 (0.026)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Material cost: Industry material cost (log)
#======================================================================================================================#
reg_matcost <- did_preliminary(
  data = triQc,
  depvar = "l.matcost",
  ATT = "e.treated",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(reg_matcost, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.matcost",
  G = "fips.code.fe",
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
reg_matcost <- dynamic_did_preliminary(
  data = triQc,
  depvar = "l.matcost",
  relative_year = "rel.year",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(reg_matcost, digits = 3, digits.stats = 3)
iplot(reg_matcost, xlim = c(-3, 3), ylim = c(-0.6, 0.6), col = "blue",
	  main = "Material Cost (log)", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_matcost)[grep(pattern = "rel.year", names(coef(reg_matcost)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_matcost, paste0(names(pre_treat_coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_matcost <- sdid_preliminary(
  data = triQc,
  depvar = "l.matcost",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(sdid_matcost, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_matcost, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_matcost, digits.stats = 3, digits = 3)

sdid_matcost <- dynamic_sdid_preliminary(
  data = triQc,
  depvar = "l.matcost",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_matcost, digits.stats = 3, digits = 3)
iplot(
  list(sdid_matcost, reg_matcost),
  xlim = c(-3, 3), ylim = c(-0.6, 0.6), col = c("blue", "pink"),
  main = "Material Cost (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.129* (0.069)", "TWFE ATT: 0.105** (0.047)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_industry_costs.pdf", width = 16, height = 4)
par(mfrow = c(1, 3))
iplot(
  list(sdid_wages, reg_wagephr),
  xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = c("blue", "pink"),
  main = "Hourly wage (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: $0.889* (0.452)", "TWFE ATT: $0.606* (0.325)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_pay, reg_pay),
  xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = c("blue", "pink"),
  main = "Total Payroll (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.043* (0.025)", "TWFE ATT: 0.035 (0.026)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_matcost, reg_matcost),
  xlim = c(-3, 3), ylim = c(-0.6, 0.6), col = c("blue", "pink"),
  main = "Material Cost (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.1291* (0.069)", "TWFE ATT: 0.105** (0.047)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
dev.off()
#======================================================================================================================#
### Low skilled vs high skilled workers---Approximated
#======================================================================================================================#
# Low skilled are approximated by using industry workers in each manufacturing facility whose wages are below the 30th
# percentile.
#======================================================================================================================#
triQc <- triQc %>%
  group_by(naics.code, facility.id) %>%
  mutate(
	wage.range = quantile(wage.perhr, probs = 0.30),
	low.skilled.workers = ifelse(test = (wage.perhr < wage.range), yes = 1, no = 0),
  ) %>%
  ungroup()
sum_up(triQc, c(wage.range, low.skilled.workers, wage.perhr))
#======================================================================================================================#
### Wage per hour for low-skilled vs high-skilled workers
#======================================================================================================================#
sdid_wages_lowskilled <- sdid_preliminary_heter_1(
  data = triQc,
  depvar = "l.wage.perhr",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_wages_lowskilled, digits.stats = 3, digits = 3)
etable(sdid_wages_lowskilled, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_wages_lowskilled, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_wages_lowskilled, xlim = c(-3, 3), ylim = c(-0.5, 0.7), col = c("blue", "pink"),
	  main = "Hourly wage (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_wages_highskilled <- sdid_preliminary_heter_0(
  data = triQc,
  depvar = "l.wage.perhr",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_wages_highskilled, digits.stats = 3, digits = 3)
iplot(sdid_wages_highskilled, xlim = c(-3, 3), ylim = c(-0.05, 0.15), col = c("blue", "pink"),
	  main = "Hourly wage (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Labour cost: Industry Pay---Total payroll for low-skilled vs high-skilled workers
#======================================================================================================================#
sdid_pay_lowskilled <- sdid_preliminary_heter_1(
  data = triQc,
  depvar = "l.pay",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_pay_lowskilled, digits.stats = 3, digits = 3)
etable(sdid_pay_lowskilled, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_pay_lowskilled, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_pay_lowskilled, xlim = c(-3, 3), ylim = c(-2, 1.5), col = c("blue", "pink"),
	  main = "Total Payroll (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_pay_highskilled <- sdid_preliminary_heter_0(
  data = triQc,
  depvar = "l.pay",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_pay_highskilled, digits.stats = 3, digits = 3)
iplot(sdid_pay_highskilled, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = c("blue", "pink"),
	  main = "Total Payroll (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_industry_costs_skilled.pdf", width = 16, height = 4)
par(mfrow = c(1, 4))
iplot(sdid_wages_lowskilled, xlim = c(-3, 3), ylim = c(-0.5, 0.7), col = c("blue", "pink"),
	  main = "Hourly wage (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_wages_highskilled, xlim = c(-3, 3), ylim = c(-0.05, 0.15), col = c("blue", "pink"),
	  main = "Hourly wage (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_pay_lowskilled, xlim = c(-3, 3), ylim = c(-2, 1.5), col = c("blue", "pink"),
	  main = "Total Payroll (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_pay_highskilled, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = c("blue", "pink"),
	  main = "Total Payroll (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Industry: Employment
#======================================================================================================================#
reg_emp <- did_preliminary(
  data = triQc,
  depvar = "l.emp",
  ATT = "e.treated",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(reg_emp, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.emp",
  G = "fips.code.fe",
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
reg_emp <- dynamic_did_preliminary(
  data = triQc,
  depvar = "l.emp",
  relative_year = "rel.year",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(reg_emp, digits = 3, digits.stats = 3)
iplot(reg_emp, xlim = c(-3, 3), ylim = c(-0.32, 0.2), col = "blue",
	  main = "Industry Employment (log)", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_emp)[grep(pattern = "rel.year", names(coef(reg_emp)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_emp, paste0(names(pre_treat_coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_emp <- sdid_preliminary(
  data = triQc,
  depvar = "l.emp",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_emp, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_emp, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_emp, digits.stats = 3, digits = 3)

sdid_emp <- dynamic_sdid_preliminary(
  data = triQc,
  depvar = "l.emp",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_emp, digits.stats = 3, digits = 3)
iplot(
  list(sdid_emp, reg_emp),
  xlim = c(-3, 3), ylim = c(-0.35, 0.2), col = c("blue", "pink"),
  main = "Industry Employment (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.002 (0.025)", "TWFE ATT: 0.000 (0.022)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Industry: Production workers
#======================================================================================================================#
reg_prodworkers <- did_preliminary(
  data = triQc,
  depvar = "l.prode",
  ATT = "e.treated",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(reg_prodworkers, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.prode",
  G = "fips.code.fe",
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
reg_prodworkers <- dynamic_did_preliminary(
  data = triQc,
  depvar = "l.prode",
  relative_year = "rel.year",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(reg_prodworkers, digits = 3, digits.stats = 3)
iplot(reg_prodworkers, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = "blue",
	  main = "Production Workers (log)", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_prodworkers)[grep(pattern = "rel.year", names(coef(reg_prodworkers)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_prodworkers, paste0(names(pre_treat_coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_prodworkers <- sdid_preliminary(
  data = triQc,
  depvar = "l.prode",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(sdid_prodworkers, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_prodworkers, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_prodworkers, digits.stats = 3, digits = 3)

sdid_prodworkers <- dynamic_sdid_preliminary(
  data = triQc,
  depvar = "l.prode",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_prodworkers, digits.stats = 3, digits = 3)
etable(sdid_prodworkers, digits.stats = 3, digits = 3)
iplot(
  list(sdid_prodworkers, reg_prodworkers),
  xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = c("blue", "pink"),
  main = "Production Workers (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.023 (0.033)", "TWFE ATT: -0.014 (0.022)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Industry: Production hours
#======================================================================================================================#
reg_phours <- did_preliminary(
  data = triQc,
  depvar = "l.prodh",
  ATT = "e.treated",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(reg_phours, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.prodh",
  G = "fips.code.fe",
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
reg_phours <- dynamic_did_preliminary(
  data = triQc,
  depvar = "l.prodh",
  relative_year = "rel.year",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(reg_phours, digits = 3, digits.stats = 3)
iplot(reg_phours, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
	  main = "Workers' Hours (log)", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_phours)[grep(pattern = "rel.year", names(coef(reg_phours)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_phours, paste0(names(pre_treat_coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_phours <- sdid_preliminary(
  data = triQc,
  depvar = "l.prodh",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(sdid_phours, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_phours, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_phours, digits.stats = 3, digits = 3)

sdid_phours <- dynamic_sdid_preliminary(
  data = triQc,
  depvar = "l.prodh",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_phours, digits.stats = 3, digits = 3)
iplot(
  list(sdid_phours, reg_phours),
  xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = c("blue", "pink"),
  main = "Production Hours (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.019 (0.033)", "TWFE ATT: -0.015 (0.024)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_emp_hours.pdf", width = 17, height = 5)
par(mfrow = c(1, 3))
iplot(
  list(sdid_emp, reg_emp),
  xlim = c(-3, 3), ylim = c(-0.35, 0.2), col = c("blue", "pink"),
  main = "Industry Employment (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.002 (0.025)", "TWFE ATT: 0.000 (0.022)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_prodworkers, reg_prodworkers),
  xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = c("blue", "pink"),
  main = "Production Workers (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.0233 (0.033)", "TWFE ATT: -0.014 (0.022)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_phours, reg_phours),
  xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = c("blue", "pink"),
  main = "Production Hours (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.019 (0.033)", "TWFE ATT: -0.015 (0.024)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
dev.off()
#======================================================================================================================#
### Employment for low-skilled vs high-skilled workers
#======================================================================================================================#
sdid_emp_lowskilled <- sdid_preliminary_heter_1(
  data = triQc,
  depvar = "l.emp",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_emp_lowskilled, digits.stats = 3, digits = 3)
etable(sdid_emp_lowskilled, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_emp_lowskilled, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_emp_lowskilled, xlim = c(-3, 3), ylim = c(-1.5, 0.7), col = c("blue", "pink"),
	  main = "Employment (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_emp_highskilled <- sdid_preliminary_heter_0(
  data = triQc,
  depvar = "l.emp",
  interact_var = "high.skilled.workers",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_emp_highskilled, digits.stats = 3, digits = 3)
iplot(sdid_emp_highskilled, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = c("blue", "pink"),
	  main = "Employment (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Production workers for low-skilled vs high-skilled workers
#======================================================================================================================#
sdid_prodworkers_lowskilled <- sdid_preliminary_heter_1(
  data = triQc,
  depvar = "l.prodw",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_prodworkers_lowskilled, digits.stats = 3, digits = 3)
etable(sdid_prodworkers_lowskilled, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_prodworkers_lowskilled, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_prodworkers_lowskilled, xlim = c(-3, 3), ylim = c(-1.8, 1.5), col = c("blue", "pink"),
	  main = "Production Workers (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_prodworkers_highskilled <- sdid_preliminary_heter_0(
  data = triQc,
  depvar = "l.prodw",
  interact_var = "high.skilled.workers",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_prodworkers_highskilled, digits.stats = 3, digits = 3)
iplot(sdid_prodworkers_highskilled, xlim = c(-3, 3), ylim = c(-0.35, 0.2), col = c("blue", "pink"),
	  main = "Production Workers (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Production Hours for low-skilled vs high-skilled workers
#======================================================================================================================#
sdid_prodhours_lowskilled <- sdid_preliminary_heter_1(
  data = triQc,
  depvar = "l.prodh",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_prodhours_lowskilled, digits.stats = 3, digits = 3)
etable(sdid_prodhours_lowskilled, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_prodhours_lowskilled, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_prodhours_lowskilled, xlim = c(-3, 3), ylim = c(-1.5, 0.85), col = c("blue", "pink"),
	  main = "Production Hours (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_prodhours_highskilled <- sdid_preliminary_heter_0(
  data = triQc,
  depvar = "l.prodh",
  interact_var = "high.skilled.workers",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_prodhours_highskilled, digits.stats = 3, digits = 3)
iplot(sdid_prodhours_highskilled, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = c("blue", "pink"),
	  main = "Production Hours (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_emp_hours_skilled.pdf", width = 12, height = 6)
par(mfrow = c(2, 3))
iplot(sdid_emp_lowskilled, xlim = c(-3, 3), ylim = c(-1.5, 0.7), col = c("blue", "pink"),
	  main = "Employment (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_emp_highskilled, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = c("blue", "pink"),
	  main = "Employment (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodworkers_lowskilled, xlim = c(-3, 3), ylim = c(-1.8, 1.5), col = c("blue", "pink"),
	  main = "Production Workers (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodworkers_highskilled, xlim = c(-3, 3), ylim = c(-0.35, 0.2), col = c("blue", "pink"),
	  main = "Production Workers (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodhours_lowskilled, xlim = c(-3, 3), ylim = c(-1.5, 0.85), col = c("blue", "pink"),
	  main = "Production Hours (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodhours_highskilled, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = c("blue", "pink"),
	  main = "Production Hours (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Industry: Industry Output
#======================================================================================================================#
reg_output <- did_preliminary(
  data = triQc,
  depvar = "l.output",
  ATT = "e.treated",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(reg_output, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.vadd",
  G = "fips.code.fe",
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
reg_output <- dynamic_did_preliminary(
  data = triQc,
  depvar = "l.output",
  relative_year = "rel.year",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(reg_output, digits = 3, digits.stats = 3)
iplot(reg_output, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Industry Output (log)", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_output)[grep(pattern = "rel.year", names(coef(reg_output)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_output, paste0(names(pre_treat_coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_output <- sdid_preliminary(
  data = triQc,
  depvar = "l.output",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(sdid_output, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_output, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_output, digits.stats = 3, digits = 3)

sdid_output <- dynamic_sdid_preliminary(
  data = triQc,
  depvar = "l.output",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_output, digits.stats = 3, digits = 3)
iplot(
  list(sdid_output, reg_output),
  xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = c("blue", "pink"),
  main = "Industry Output (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.125*** (0.032)", "TWFE ATT: 0.117*** (0.037)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Industry: Output per hour
#======================================================================================================================#
reg_outputprhr <- did_preliminary(
  data = triQc,
  depvar = "l.output.perhr",
  ATT = "e.treated",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(reg_outputprhr, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.output.perhr",
  G = "fips.code.fe",
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
reg_outputprhr <- dynamic_did_preliminary(
  data = triQc,
  depvar = "l.output.perhr",
  relative_year = "rel.year",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(reg_outputprhr, digits = 3, digits.stats = 3)
iplot(reg_outputprhr, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Output per Hour (log)", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_outputprhr)[grep(pattern = "rel.year", names(coef(reg_outputprhr)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_outputprhr, paste0(names(pre_treat_coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_outputprhr <- sdid_preliminary(
  data = triQc,
  depvar = "l.output.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(sdid_outputprhr, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_outputprhr, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_outputprhr, digits.stats = 3, digits = 3)

sdid_outputprhr <- dynamic_sdid_preliminary(
  data = triQc,
  depvar = "l.output.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_outputprhr, digits.stats = 3, digits = 3)
iplot(
  list(sdid_outputprhr, reg_outputprhr),
  xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = c("blue", "pink"),
  main = "Output per Hour (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.144*** (0.038)", "TWFE ATT: 0.132*** (0.030)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Industry: Output per Worker
#======================================================================================================================#
reg_outputperworker <- did_preliminary(
  data = triQc,
  depvar = "l.output.perworker",
  ATT = "e.treated",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(reg_outputperworker, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.output.perworker",
  G = "fips.code.fe",
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
reg_outputperworker <- dynamic_did_preliminary(
  data = triQc,
  depvar = "l.output.perworker",
  relative_year = "rel.year",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(reg_outputperworker, digits = 3, digits.stats = 3)
iplot(reg_outputperworker, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Output per Worker (log)", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_outputperworker)[grep(pattern = "rel.year", names(coef(reg_outputperworker)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_outputperworker, paste0(names(pre_treat_coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_outputperworker <- sdid_preliminary(
  data = triQc,
  depvar = "l.output.perworker",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(sdid_outputperworker, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_outputperworker, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_outputperworker, digits.stats = 3, digits = 3)

sdid_outputperworker <- dynamic_sdid_preliminary(
  data = triQc,
  depvar = "l.output.perworker",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_outputperworker, digits.stats = 3, digits = 3)
iplot(
  list(sdid_outputperworker, reg_outputperworker),
  xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = c("blue", "pink"),
  main = "Output per Worker (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.127*** (0.032)", "TWFE ATT: 0.117*** (0.025)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_output.pdf", width = 16, height = 4)
par(mfrow = c(1, 3))
iplot(
  list(sdid_output, reg_output),
  xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = c("blue", "pink"),
  main = "Industry Output (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
# Add a legend to the plot
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.125*** (0.032)", "TWFE ATT: 0.117*** (0.037)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_outputprhr, reg_outputprhr),
  xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = c("blue", "pink"),
  main = "Output per Hour (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
# Add a legend to the plot
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.144*** (0.038)", "TWFE ATT: 0.132*** (0.030)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_outputperworker, reg_outputperworker),
  xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = c("blue", "pink"),
  main = "Output per Worker (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
# Add a legend to the plot
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.127*** (0.032)", "TWFE ATT: 0.117*** (0.025)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
dev.off()
#======================================================================================================================#
### Profits
#======================================================================================================================#
triQc <- triQc %>%
  group_by(year, naics.code) %>%
  rename(revenue = vship) %>%
  mutate(
	total.cost = matcost + prodw + energy,
	profit = revenue - total.cost,
	l.profit = log(profit),
	revenue.to.profit = (revenue / profit) * 100,
	l.profitmargin = log(revenue.to.profit)
  )
#----------------------------------------------------------------------------------------------------------------------#
reg_profit <- did_preliminary(
  data = triQc,
  depvar = "l.profit",
  ATT = "e.treated",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(reg_profit, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.profit",
  G = "fips.code.fe",
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
reg_profit <- dynamic_did_preliminary(
  data = triQc,
  depvar = "l.profit",
  relative_year = "rel.year",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(reg_profit, digits = 3, digits.stats = 3)
iplot(reg_profit, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Industry Profits (log)", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_profit)[grep(pattern = "rel.year", names(coef(reg_profit)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_profit, paste0(names(pre_treat_coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_profit <- sdid_preliminary(
  data = triQc,
  depvar = "l.profit",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(sdid_profit, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_profit, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_profit, digits.stats = 3, digits = 3)

sdid_profit <- dynamic_sdid_preliminary(
  data = triQc,
  depvar = "l.profit",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_profit, digits.stats = 3, digits = 3)
iplot(
  list(reg_profit, sdid_profit),
  xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = c("blue", "pink"),
  main = "Industry Profits (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.156*** (0.034)", "TWFE ATT: 0.148*** (0.041)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Profit margin
#======================================================================================================================#
reg_profit_margin <- did_preliminary(
  data = triQc,
  depvar = "l.profitmargin",
  ATT = "e.treated",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(reg_profit_margin, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.profitmargin",
  G = "fips.code.fe",
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
reg_profit_margin <- dynamic_did_preliminary(
  data = triQc,
  depvar = "l.profitmargin",
  relative_year = "rel.year",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(reg_profit_margin, digits = 3, digits.stats = 3)
iplot(reg_profit_margin, xlim = c(-3, 3), ylim = c(-0.32, 0.2), col = "blue",
	  main = "Profit Margin (log)", xlab = "relative year",
	  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_profit_margin)[grep(pattern = "rel.year", names(coef(reg_profit_margin)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_profit_margin, paste0(names(pre_treat_coef), " = 0"), test = "F")
#----------------------------------------------------------------------------------------------------------------------#
# Sun and Abraham (2020)
#----------------------------------------------------------------------------------------------------------------------#
sdid_profit_margin <- sdid_preliminary(
  data = triQc,
  depvar = "l.profitmargin",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  did_county_fes = did_county_fes
)
etable(sdid_profit_margin, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_profit_margin, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_profit_margin, digits.stats = 3, digits = 3)

sdid_profit_margin <- dynamic_sdid_preliminary(
  data = triQc,
  depvar = "l.profitmargin",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  county_fes = county_fes
)
etable(sdid_profit_margin, digits.stats = 3, digits = 3)
iplot(
  list(reg_profit_margin, sdid_profit_margin),
  xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = c("blue", "pink"),
  main = "Profit Margin (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.027 (0.034)", "TWFE ATT: -0.035 (0.024)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_profits.pdf", width = 15, height = 5)
par(mfrow = c(1, 2))
iplot(
  list(reg_profit, sdid_profit),
  xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = c("blue", "pink"),
  main = "Industry Profits (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.156*** (0.034)", "TWFE ATT: 0.148*** (0.041)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(reg_profit_margin, sdid_profit_margin),
  xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = c("blue", "pink"),
  main = "Profit Margin (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.027 (0.034)", "TWFE ATT: -0.035 (0.024)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
dev.off()
#======================================================================================================================#