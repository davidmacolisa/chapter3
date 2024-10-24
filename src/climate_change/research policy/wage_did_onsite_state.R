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
setwd(dir = "your_path")
#======================================================================================================================#
### Loading Data and Functions
#======================================================================================================================#
source(file = "your_path/functions.R", echo = T)
file <- "your_path/triQc_onsite_econj.rds"
triQs <- read_rds(file = file)
#======================================================================================================================#
### Labour Cost---Wage per hr, weekly wages, and  total wages
#======================================================================================================================#
table(triQs$facility.state, triQs$ch.year)
n_distinct(triQs$chemical.name)
sort(unique(triQs$chemical.name))
sort(unique(triQs$year))
sort(unique(triQs$rel.year))
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
	  facility.state.fe,
	  border.state.fe,
	  border.state.year.fe
	)
  ,
  data = triQs,
  cluster = ~facility.state,
)
etable(treat_sel, digits = 3, digits.stats = 3)
#======================================================================================================================#
### Wage per hour
#======================================================================================================================#
reg_wagephr <- did_baseline(
  data = triQs,
  depvar = "wage.perhr",
  ATT = "e.treated",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(reg_wagephr, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.wage.perhr",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQs,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_wagephr <- dynamic_did_baseline(
  data = triQs,
  depvar = "l.wage.perhr",
  relative_year = "rel.year",
  cluster = ~facility.state,
  fes = state_fes()
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
sdid_wages <- sdid_baseline(
  data = triQs,
  depvar = "wage.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(sdid_wages, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_wages, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_wages, digits.stats = 3, digits = 3)

sdid_wages <- dynamic_sdid_baseline(
  data = triQs,
  depvar = "l.wage.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = state_fes()
)
etable(sdid_wages, digits.stats = 3, digits = 3)
iplot(list(sdid_wages, reg_wagephr),
	  xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = c("blue", "pink"),
	  main = "Hourly wage (log)", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: $0.704 (0.508)", "TWFE ATT: $0.894** (0.366)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Labour cost: Industry Pay---Total payroll
#======================================================================================================================#
reg_pay <- did_baseline(
  data = triQs,
  depvar = "l.pay",
  ATT = "e.treated",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(reg_pay, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.pay",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQs,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_pay <- dynamic_did_baseline(
  data = triQs,
  depvar = "l.pay",
  relative_year = "rel.year",
  cluster = ~facility.state,
  fes = state_fes()
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
sdid_pay <- sdid_baseline(
  data = triQs,
  depvar = "l.pay",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(sdid_pay, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_pay, agg = "cohort", digits = 3, digits.stats = 3)
etable(sdid_pay, digits.stats = 3, digits = 3)

sdid_pay <- dynamic_sdid_baseline(
  data = triQs,
  depvar = "l.pay",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = state_fes()
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
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.040 (0.046)", "TWFE ATT: 0.067** (0.031)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Material cost: Industry material cost (log)
#======================================================================================================================#
reg_matcost <- did_baseline(
  data = triQs,
  depvar = "l.matcost",
  ATT = "e.treated",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(reg_matcost, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.matcost",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQs,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_matcost <- dynamic_did_baseline(
  data = triQs,
  depvar = "l.matcost",
  relative_year = "rel.year",
  cluster = ~facility.state,
  fes = state_fes()
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
sdid_matcost <- sdid_baseline(
  data = triQs,
  depvar = "l.matcost",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(sdid_matcost, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_matcost, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_matcost, digits.stats = 3, digits = 3)

sdid_matcost <- dynamic_sdid_baseline(
  data = triQs,
  depvar = "l.matcost",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = state_fes()
)
etable(sdid_matcost, digits.stats = 3, digits = 3)
iplot(
  list(sdid_matcost, reg_matcost),
  xlim = c(-3, 3), ylim = c(-1.1, 0.6), col = c("blue", "pink"),
  main = "Material Cost (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.081 (0.110)", "TWFE ATT:  0.169** (0.060)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_industry_costs_state.pdf", width = 16, height = 4)
par(mfrow = c(1, 3))
iplot(
  list(sdid_wages, reg_wagephr),
  xlim = c(-3, 3), ylim = c(-0.2, 0.15), col = c("blue", "pink"),
  main = "Hourly wage (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: $0.704 (0.508)", "TWFE ATT: $0.894** (0.366)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_pay, reg_pay),
  xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = c("blue", "pink"),
  main = "Total Payroll (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.040 (0.046)", "TWFE ATT: 0.067** (0.031)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_matcost, reg_matcost),
  xlim = c(-3, 3), ylim = c(-1.1, 0.6), col = c("blue", "pink"),
  main = "Material Cost (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.081 (0.110)", "TWFE ATT:  0.169** (0.060)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
dev.off()
#======================================================================================================================#
### Industry: Employment
#======================================================================================================================#
reg_emp <- did_baseline(
  data = triQs,
  depvar = "l.emp",
  ATT = "e.treated",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(reg_emp, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.emp",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQs,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_emp <- dynamic_did_baseline(
  data = triQs,
  depvar = "l.emp",
  relative_year = "rel.year",
  cluster = ~facility.state,
  fes = state_fes()
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
sdid_emp <- sdid_baseline(
  data = triQs,
  depvar = "l.emp",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(sdid_emp, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_emp, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_emp, digits.stats = 3, digits = 3)

sdid_emp <- dynamic_sdid_baseline(
  data = triQs,
  depvar = "l.emp",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = state_fes()
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
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.002 (0.039)", "TWFE ATT: 0.025 (0.030)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Industry: Production workers
#======================================================================================================================#
reg_prodworkers <- did_baseline(
  data = triQs,
  depvar = "l.prode",
  ATT = "e.treated",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(reg_prodworkers, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.prode",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQs,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_prodworkers <- dynamic_did_baseline(
  data = triQs,
  depvar = "l.prode",
  relative_year = "rel.year",
  cluster = ~facility.state,
  fes = state_fes()
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
sdid_prodworkers <- sdid_baseline(
  data = triQs,
  depvar = "l.prode",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(sdid_prodworkers, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_prodworkers, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_prodworkers, digits.stats = 3, digits = 3)

sdid_prodworkers <- dynamic_sdid_baseline(
  data = triQs,
  depvar = "l.prode",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = state_fes()
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
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.029 (0.039)", "TWFE ATT: 0.005 (0.028)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Industry: Production hours
#======================================================================================================================#
reg_phours <- did_baseline(
  data = triQs,
  depvar = "l.prodh",
  ATT = "e.treated",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(reg_phours, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.prodh",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQs,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_phours <- dynamic_did_baseline(
  data = triQs,
  depvar = "l.prodh",
  relative_year = "rel.year",
  cluster = ~facility.state,
  fes = state_fes()
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
sdid_phours <- sdid_baseline(
  data = triQs,
  depvar = "l.prodh",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(sdid_phours, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_phours, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_phours, digits.stats = 3, digits = 3)

sdid_phours <- dynamic_sdid_baseline(
  data = triQs,
  depvar = "l.prodh",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = state_fes()
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
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.027 (0.039)", "TWFE ATT: 0.005 (0.028)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_emp_hours_state.pdf", width = 17, height = 5)
par(mfrow = c(1, 3))
iplot(
  list(sdid_emp, reg_emp),
  xlim = c(-3, 3), ylim = c(-0.35, 0.2), col = c("blue", "pink"),
  main = "Industry Employment (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.002 (0.039)", "TWFE ATT: 0.025 (0.030)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_prodworkers, reg_prodworkers),
  xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = c("blue", "pink"),
  main = "Production Workers (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.029 (0.039)", "TWFE ATT: 0.005 (0.028)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(sdid_phours, reg_phours),
  xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = c("blue", "pink"),
  main = "Production Hours (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.027 (0.039)", "TWFE ATT: 0.005 (0.028)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
dev.off()
#======================================================================================================================#
### Industry: Industry Output
#======================================================================================================================#
reg_output <- did_baseline(
  data = triQs,
  depvar = "l.output",
  ATT = "e.treated",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(reg_output, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.output",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQs,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_output <- dynamic_did_baseline(
  data = triQs,
  depvar = "l.output",
  relative_year = "rel.year",
  cluster = ~facility.state,
  fes = state_fes()
)
etable(reg_output, digits = 3, digits.stats = 3)
iplot(reg_output, xlim = c(-6, 5), ylim = c(-0.3, 0.3), col = "blue",
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
sdid_output <- sdid_baseline(
  data = triQs,
  depvar = "l.output",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(sdid_output, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_output, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_output, digits.stats = 3, digits = 3)

sdid_output <- dynamic_sdid_baseline(
  data = triQs,
  depvar = "l.output",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = state_fes()
)
etable(sdid_output, digits.stats = 3, digits = 3)
iplot(
  list(sdid_output, reg_output),
  xlim = c(-3, 3), ylim = c(-0.6, 0.5), col = c("blue", "pink"),
  main = "Industry Output (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.130* (0.069)", "TWFE ATT: 0.156*** (0.055)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Industry: Output per hour
#======================================================================================================================#
reg_outputprhr <- did_baseline(
  data = triQs,
  depvar = "l.output.perhr",
  ATT = "e.treated",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(reg_outputprhr, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.output.perhr",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQs,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_outputprhr <- dynamic_did_baseline(
  data = triQs,
  depvar = "l.output.perhr",
  relative_year = "rel.year",
  cluster = ~facility.state,
  fes = state_fes()
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
sdid_outputprhr <- sdid_baseline(
  data = triQs,
  depvar = "l.output.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(sdid_outputprhr, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_outputprhr, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_outputprhr, digits.stats = 3, digits = 3)

sdid_outputprhr <- dynamic_sdid_baseline(
  data = triQs,
  depvar = "l.output.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = state_fes()
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
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.157** (0.058)", "TWFE ATT: 0.151*** (0.044)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Industry: Output per Worker
#======================================================================================================================#
reg_outputperworker <- did_baseline(
  data = triQs,
  depvar = "l.output.perworker",
  ATT = "e.treated",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(reg_outputperworker, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.output.perworker",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQs,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_outputperworker <- dynamic_did_baseline(
  data = triQs,
  depvar = "l.output.perworker",
  relative_year = "rel.year",
  cluster = ~facility.state,
  fes = state_fes()
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
sdid_outputperworker <- sdid_baseline(
  data = triQs,
  depvar = "l.output.perworker",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(sdid_outputperworker, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_outputperworker, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_outputperworker, digits.stats = 3, digits = 3)

sdid_outputperworker <- dynamic_sdid_baseline(
  data = triQs,
  depvar = "l.output.perworker",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = state_fes()
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
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.132** (0.051)", "TWFE ATT: 0.131*** (0.035)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_output_state.pdf", width = 16, height = 4)
par(mfrow = c(1, 3))
iplot(
  list(sdid_output, reg_output),
  xlim = c(-3, 3), ylim = c(-0.6, 0.5), col = c("blue", "pink"),
  main = "Industry Output (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
# Add a legend to the plot
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.130* (0.069)", "TWFE ATT: 0.156*** (0.055)"),
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
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.157** (0.058)", "TWFE ATT: 0.151*** (0.044)"),
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
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.132** (0.051)", "TWFE ATT: 0.131*** (0.035)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
dev.off()
#======================================================================================================================#
### Profits
#======================================================================================================================#
triQs <- triQs %>%
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
reg_profit <- did_baseline(
  data = triQs,
  depvar = "l.profit",
  ATT = "e.treated",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(reg_profit, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.profit",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQs,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_profit <- dynamic_did_baseline(
  data = triQs,
  depvar = "l.profit",
  relative_year = "rel.year",
  cluster = ~facility.state,
  fes = state_fes()
)
etable(reg_profit, digits = 3, digits.stats = 3)
iplot(reg_profit, xlim = c(-4, 3), ylim = c(-0.2, 0.4), col = "blue",
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
sdid_profit <- sdid_baseline(
  data = triQs,
  depvar = "l.profit",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(sdid_profit, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_profit, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_profit, digits.stats = 3, digits = 3)

sdid_profit <- dynamic_sdid_baseline(
  data = triQs,
  depvar = "l.profit",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = state_fes()
)
etable(sdid_profit, digits.stats = 3, digits = 3)
iplot(
  list(reg_profit, sdid_profit),
  xlim = c(-3, 3), ylim = c(-0.6, 0.4), col = c("blue", "pink"),
  main = "Industry Profits (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.170** (0.074)", "TWFE ATT: 0.191*** (0.065)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
### Profit margin
#======================================================================================================================#
reg_profit_margin <- did_baseline(
  data = triQs,
  depvar = "l.profitmargin",
  ATT = "e.treated",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(reg_profit_margin, digits = 3, digits.stats = 3)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.profitmargin",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQs,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_profit_margin <- dynamic_did_baseline(
  data = triQs,
  depvar = "l.profitmargin",
  relative_year = "rel.year",
  cluster = ~facility.state,
  fes = state_fes()
)
etable(reg_profit_margin, digits = 3, digits.stats = 3)
iplot(reg_profit_margin, xlim = c(-4, 3), ylim = c(-0.32, 0.2), col = "blue",
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
sdid_profit_margin <- sdid_baseline(
  data = triQs,
  depvar = "l.profitmargin",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(sdid_profit_margin, agg = "ATT", digits.stats = 3, digits = 3)
etable(sdid_profit_margin, agg = "cohort", digits.stats = 3, digits = 3)
etable(sdid_profit_margin, digits.stats = 3, digits = 3)

sdid_profit_margin <- dynamic_sdid_baseline(
  data = triQs,
  depvar = "l.profitmargin",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.state,
  fes = state_fes()
)
etable(sdid_profit_margin, digits.stats = 3, digits = 3)
iplot(
  list(reg_profit_margin, sdid_profit_margin),
  xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = c("blue", "pink"),
  main = "Profit Margin (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.074* (0.040)", "TWFE ATT: -0.027 (0.040)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_profits_state.pdf", width = 15, height = 5)
par(mfrow = c(1, 2))
iplot(
  list(reg_profit, sdid_profit),
  xlim = c(-3, 3), ylim = c(-0.6, 0.4), col = c("blue", "pink"),
  main = "Industry Profits (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: 0.170** (0.074)", "TWFE ATT: 0.191*** (0.065)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
iplot(
  list(reg_profit_margin, sdid_profit_margin),
  xlim = c(-3, 3), ylim = c(-0.5, 0.2), col = c("blue", "pink"),
  main = "Profit Margin (log)", xlab = "relative year", lwd = 1, cex = 4,
  pt.cex = 1.5, pt.col = c("red", "black"), pt.join = T,
  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
legend(x = "bottomright", legend = c("Sun and Abraham (2020) ATT: -0.074* (0.040)", "TWFE ATT: -0.027 (0.040)"),
	   col = c("red", "black"), pch = 19, pt.cex = 2, bty = "n")
dev.off()
#======================================================================================================================#