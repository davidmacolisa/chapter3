#======================================================================================================================#
### PhD Chapter 3
### Unforseen Minimum Wage Consequences
### 30 October 2023
### Use Staggered DID with border-county identification
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
### Triple Differences
#======================================================================================================================#
### Low skilled vs high skilled workers---Approximated
#======================================================================================================================#
# Low skilled are approximated by using industry workers in each manufacturing facility whose wages are below the 30th
# percentile.
#======================================================================================================================#
triQc <- triQc %>%
  group_by(naics.code, facility.id) %>%
  mutate(
	wage.range = quantile(wage.perhr, probs = 0.3),
	low.skilled.workers = ifelse(test = (wage.perhr < wage.range), yes = 1, no = 0),
  ) %>%
  ungroup()
sum_up(triQc, c(low.skilled.workers, wage.perhr))
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
### Wage per hour for low-skilled vs high-skilled workers
#======================================================================================================================#
sdid_wages_lowskilled <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.wage.perhr",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_wages_lowskilled, digits.stats = 3, digits = 3)
etable(sdid_wages_lowskilled, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_wages_lowskilled, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_wages_lowskilled, xlim = c(-3, 3), ylim = c(-0.5, 0.7), col = "blue",
	  main = "Hourly wage (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_wages_highskilled <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.wage.perhr",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_wages_highskilled, digits.stats = 3, digits = 3)
iplot(sdid_wages_highskilled, xlim = c(-3, 3), ylim = c(-0.05, 0.15), col = "blue",
	  main = "Hourly wage (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Wage per hour for labour- v. capital-intensive industries
#======================================================================================================================#
sdid_wages_li <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.wage.perhr",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_wages_li, digits.stats = 3, digits = 3)
etable(sdid_wages_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_wages_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_wages_li, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Hourly wage (log), Labour-Intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_wages_ci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.wage.perhr",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_wages_ci, digits.stats = 3, digits = 3)
iplot(sdid_wages_ci, xlim = c(-3, 3), ylim = c(-0.05, 0.1), col = "blue",
	  main = "Hourly wage (log), Capital-Intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Wage per hour for high- v. low-profit industries
#======================================================================================================================#
sdid_wages_hp <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.wage.perhr",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_wages_hp, digits.stats = 3, digits = 3)
etable(sdid_wages_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_wages_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_wages_hp, xlim = c(-3, 3), ylim = c(-0.15, 0.35), col = "blue",
	  main = "Hourly wage (log), High-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_wages_lp <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.wage.perhr",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_wages_lp, digits.stats = 3, digits = 3)
iplot(sdid_wages_lp, xlim = c(-3, 3), ylim = c(-0.05, 0.1), col = "blue",
	  main = "Hourly wage (log), Low-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Labour cost: Industry Pay---Total payroll for low- vs high-skilled workers
#======================================================================================================================#
sdid_pay_lowskilled <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.pay",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_pay_lowskilled, digits.stats = 3, digits = 3)
etable(sdid_pay_lowskilled, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_pay_lowskilled, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_pay_lowskilled, xlim = c(-3, 3), ylim = c(-2, 1.5), col = "blue",
	  main = "Total Payroll (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_pay_highskilled <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.pay",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_pay_highskilled, digits.stats = 3, digits = 3)
iplot(sdid_pay_highskilled, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Payroll (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Labour cost: Industry Pay---Total payroll for labour- vs capital-intensive industries
#======================================================================================================================#
sdid_pay_li <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.pay",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_pay_li, digits.stats = 3, digits = 3)
etable(sdid_pay_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_pay_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_pay_li, xlim = c(-3, 3), ylim = c(-0.6, 0.7), col = "blue",
	  main = "Total Payroll (log), Labour-Intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_pay_ci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.pay",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_pay_ci, digits.stats = 3, digits = 3)
iplot(sdid_pay_ci, xlim = c(-3, 3), ylim = c(-0.25, 0.35), col = "blue",
	  main = "Total Payroll (log), Capital-Intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Labour cost: Industry Pay---Total payroll for high- vs low-profit industries
#======================================================================================================================#
sdid_pay_hp <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.pay",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_pay_hp, digits.stats = 3, digits = 3)
etable(sdid_pay_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_pay_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_pay_hp, xlim = c(-3, 3), ylim = c(-0.2, 0.8), col = "blue",
	  main = "Total Payroll (log), High-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_pay_lp <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.pay",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_pay_lp, digits.stats = 3, digits = 3)
iplot(sdid_pay_lp, xlim = c(-3, 3), ylim = c(-0.4, 0.1), col = "blue",
	  main = "Total Payroll (log), Low-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_industry_cost_heter.pdf", width = 20, height = 3.5)
par(mfrow = c(2, 6))
iplot(sdid_wages_lowskilled, xlim = c(-3, 3), ylim = c(-0.5, 0.7), col = "blue",
	  main = "Hourly wage (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_wages_highskilled, xlim = c(-3, 3), ylim = c(-0.05, 0.15), col = "blue",
	  main = "Hourly wage (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_wages_li, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Hourly wage (log), Labour-Intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_wages_ci, xlim = c(-3, 3), ylim = c(-0.05, 0.1), col = "blue",
	  main = "Hourly wage (log), Capital-Intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_wages_hp, xlim = c(-3, 3), ylim = c(-0.15, 0.35), col = "blue",
	  main = "Hourly wage (log), High-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_wages_lp, xlim = c(-3, 3), ylim = c(-0.05, 0.1), col = "blue",
	  main = "Hourly wage (log), Low-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_pay_lowskilled, xlim = c(-3, 3), ylim = c(-2, 1.5), col = "blue",
	  main = "Total Payroll (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_pay_highskilled, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Total Payroll (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_pay_li, xlim = c(-3, 3), ylim = c(-0.6, 0.7), col = "blue",
	  main = "Total Payroll (log), Labour-Intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_pay_ci, xlim = c(-3, 3), ylim = c(-0.25, 0.35), col = "blue",
	  main = "Total Payroll (log), Capital-Intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_pay_hp, xlim = c(-3, 3), ylim = c(-0.2, 0.8), col = "blue",
	  main = "Total Payroll (log), High-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_pay_lp, xlim = c(-3, 3), ylim = c(-0.4, 0.1), col = "blue",
	  main = "Total Payroll (log), Low-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Employment for low-skilled vs high-skilled workers
#======================================================================================================================#
sdid_emp_lowskilled <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.emp",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_emp_lowskilled, digits.stats = 3, digits = 3)
etable(sdid_emp_lowskilled, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_emp_lowskilled, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_emp_lowskilled, xlim = c(-3, 3), ylim = c(-1.5, 0.7), col = "blue",
	  main = "Employment (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_emp_highskilled <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.emp",
  interact_var = "high.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_emp_highskilled, digits.stats = 3, digits = 3)
iplot(sdid_emp_highskilled, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Employment (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Production workers for low-skilled vs high-skilled workers
#======================================================================================================================#
sdid_prodworkers_lowskilled <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.prodw",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodworkers_lowskilled, digits.stats = 3, digits = 3)
etable(sdid_prodworkers_lowskilled, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_prodworkers_lowskilled, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_prodworkers_lowskilled, xlim = c(-3, 3), ylim = c(-1.8, 1.5), col = "blue",
	  main = "Production Workers (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_prodworkers_highskilled <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.prodw",
  interact_var = "high.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodworkers_highskilled, digits.stats = 3, digits = 3)
iplot(sdid_prodworkers_highskilled, xlim = c(-3, 3), ylim = c(-0.35, 0.2), col = "blue",
	  main = "Production Workers (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Production Hours for low-skilled vs high-skilled workers
#======================================================================================================================#
sdid_prodhours_lowskilled <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.prodh",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodhours_lowskilled, digits.stats = 3, digits = 3)
etable(sdid_prodhours_lowskilled, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_prodhours_lowskilled, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_prodhours_lowskilled, xlim = c(-3, 3), ylim = c(-1.5, 0.85), col = "blue",
	  main = "Production Hours (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_prodhours_highskilled <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.prodh",
  interact_var = "high.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodhours_highskilled, digits.stats = 3, digits = 3)
iplot(sdid_prodhours_highskilled, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = "blue",
	  main = "Production Hours (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_emp_hours_skilled.pdf", width = 12, height = 6)
par(mfrow = c(2, 3))
iplot(sdid_emp_lowskilled, xlim = c(-3, 3), ylim = c(-1.5, 0.7), col = "blue",
	  main = "Employment (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_emp_highskilled, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Employment (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodworkers_lowskilled, xlim = c(-3, 3), ylim = c(-1.8, 1.5), col = "blue",
	  main = "Production Workers (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodworkers_highskilled, xlim = c(-3, 3), ylim = c(-0.35, 0.2), col = "blue",
	  main = "Production Workers (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodhours_lowskilled, xlim = c(-3, 3), ylim = c(-1.5, 0.85), col = "blue",
	  main = "Production Hours (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodhours_highskilled, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = "blue",
	  main = "Production Hours (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
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