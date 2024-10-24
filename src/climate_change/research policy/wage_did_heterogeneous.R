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
setwd(dir = "your_path")
#======================================================================================================================#
### Loading Data
#======================================================================================================================#
source(file = "your_path/functions.R", echo = T)
file <- "your_path/triQc_onsite_econj.rds"
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
### Profitability
### I proxy profitability by the first pre-treatment period indutry profits.
# Industries with profits lower than the median are more less profitable, and those above
# are more profitable.
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
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_industry_cost_heter.pdf", width = 20, height = 7)
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
  interact_var = "low.skilled.workers",
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
### Employment for labour- v. capital-intensive industries
#======================================================================================================================#
sdid_emp_li <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.emp",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_emp_li, digits.stats = 3, digits = 3)
etable(sdid_emp_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_emp_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_emp_li, xlim = c(-3, 3), ylim = c(-0.6, 0.7), col = "blue",
	  main = "Employment (log), Labour-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_emp_ci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.emp",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_emp_ci, digits.stats = 3, digits = 3)
iplot(sdid_emp_ci, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Employment (log), Capital-intensive ", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Employment for high- v. low-profit industries
#======================================================================================================================#
sdid_emp_hp <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.emp",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_emp_hp, digits.stats = 3, digits = 3)
etable(sdid_emp_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_emp_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_emp_hp, xlim = c(-3, 3), ylim = c(-0.3, 0.6), col = "blue",
	  main = "Employment (log), High-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_emp_lp <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.emp",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_emp_lp, digits.stats = 3, digits = 3)
iplot(sdid_emp_lp, xlim = c(-3, 3), ylim = c(-0.45, 0.1), col = "blue",
	  main = "Employment (log), Low-profit", xlab = "relative year", lwd = 1, cex = 4,
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
  interact_var = "low.skilled.workers",
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
### Production workers for labour- v. capital-intensive industries
#======================================================================================================================#
sdid_prodworkers_li <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.prodw",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodworkers_li, digits.stats = 3, digits = 3)
etable(sdid_prodworkers_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_prodworkers_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_prodworkers_li, xlim = c(-3, 3), ylim = c(-0.65, 0.6), col = "blue",
	  main = "Production Workers (log), Labour-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_prodworkers_ci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.prodw",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodworkers_ci, digits.stats = 3, digits = 3)
iplot(sdid_prodworkers_ci, xlim = c(-3, 3), ylim = c(-0.35, 0.3), col = "blue",
	  main = "Production Workers (log), Capital-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Production workers for high- v. low-profit industries
#======================================================================================================================#
sdid_prodworkers_hp <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.prodw",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodworkers_hp, digits.stats = 3, digits = 3)
etable(sdid_prodworkers_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_prodworkers_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_prodworkers_hp, xlim = c(-3, 3), ylim = c(-0.15, 0.7), col = "blue",
	  main = "Production Workers (log), High-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_prodworkers_lp <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.prodw",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodworkers_lp, digits.stats = 3, digits = 3)
iplot(sdid_prodworkers_lp, xlim = c(-3, 3), ylim = c(-0.45, 0.1), col = "blue",
	  main = "Production Workers (log), Low-Profit", xlab = "relative year", lwd = 1, cex = 4,
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
  interact_var = "low.skilled.workers",
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
### Production Hours for labour- v. capital-intensive industries
#======================================================================================================================#
sdid_prodhours_li <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.prodh",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodhours_li, digits.stats = 3, digits = 3)
etable(sdid_prodhours_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_prodhours_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_prodhours_li, xlim = c(-3, 3), ylim = c(-0.6, 0.65), col = "blue",
	  main = "Production Hours (log), Labour-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_prodhours_ci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.prodh",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodhours_ci, digits.stats = 3, digits = 3)
iplot(sdid_prodhours_ci, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
	  main = "Production Hours (log), Capital-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Production Hours for high- v. low-profit industries
#======================================================================================================================#
sdid_prodhours_hp <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.prodh",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodhours_hp, digits.stats = 3, digits = 3)
etable(sdid_prodhours_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_prodhours_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_prodhours_hp, xlim = c(-3, 3), ylim = c(-0.25, 0.5), col = "blue",
	  main = "Production Hours (log), High-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_prodhours_lp <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.prodh",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodhours_lp, digits.stats = 3, digits = 3)
iplot(sdid_prodhours_lp, xlim = c(-3, 3), ylim = c(-0.5, 0.1), col = "blue",
	  main = "Production Hours (log), Low-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_emp_hours_heter.pdf", width = 24, height = 11)
par(mfrow = c(3, 6))
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
iplot(sdid_emp_li, xlim = c(-3, 3), ylim = c(-0.6, 0.7), col = "blue",
	  main = "Employment (log), Labour-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_emp_ci, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
	  main = "Employment (log), Capital-intensive ", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_emp_hp, xlim = c(-3, 3), ylim = c(-0.3, 0.6), col = "blue",
	  main = "Employment (log), High-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

iplot(sdid_emp_lp, xlim = c(-3, 3), ylim = c(-0.45, 0.1), col = "blue",
	  main = "Employment (log), Low-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
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
iplot(sdid_prodworkers_li, xlim = c(-3, 3), ylim = c(-0.65, 0.6), col = "blue",
	  main = "Production Workers (log), Labour-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodworkers_ci, xlim = c(-3, 3), ylim = c(-0.35, 0.3), col = "blue",
	  main = "Production Workers (log), Capital-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodworkers_hp, xlim = c(-3, 3), ylim = c(-0.15, 0.7), col = "blue",
	  main = "Production Workers (log), High-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodworkers_lp, xlim = c(-3, 3), ylim = c(-0.45, 0.1), col = "blue",
	  main = "Production Workers (log), Low-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
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
iplot(sdid_prodhours_li, xlim = c(-3, 3), ylim = c(-0.6, 0.65), col = "blue",
	  main = "Production Hours (log), Labour-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodhours_ci, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
	  main = "Production Hours (log), Capital-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodhours_hp, xlim = c(-3, 3), ylim = c(-0.25, 0.5), col = "blue",
	  main = "Production Hours (log), High-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodhours_lp, xlim = c(-3, 3), ylim = c(-0.5, 0.1), col = "blue",
	  main = "Production Hours (log), Low-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Output for low- v. high-skilled workers
#======================================================================================================================#
sdid_output_lowskilled <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_lowskilled, digits.stats = 3, digits = 3)
etable(sdid_output_lowskilled, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_lowskilled, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_lowskilled, xlim = c(-3, 3), ylim = c(-2.5, 2.5), col = "blue",
	  main = "Output (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_highskilled <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_highskilled, digits.stats = 3, digits = 3)
iplot(sdid_output_highskilled, xlim = c(-3, 3), ylim = c(-0.25, 0.35), col = "blue",
	  main = "Output (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Output for labour- v. capital-intensive industries
#======================================================================================================================#
sdid_output_li <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_li, digits.stats = 3, digits = 3)
etable(sdid_output_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_li, xlim = c(-3, 3), ylim = c(-0.6, 0.7), col = "blue",
	  main = "Output (log), Labour-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_ci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_ci, digits.stats = 3, digits = 3)
iplot(sdid_output_ci, xlim = c(-3, 3), ylim = c(-0.3, 0.5), col = "blue",
	  main = "Output (log), Capital-intensive ", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Output for high- v. low-profit industries
#======================================================================================================================#
sdid_output_hp <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_hp, digits.stats = 3, digits = 3)
etable(sdid_output_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_hp, xlim = c(-3, 3), ylim = c(-0.3, 0.8), col = "blue",
	  main = "Output (log), High-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_lp <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_lp, digits.stats = 3, digits = 3)
iplot(sdid_output_lp, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Output (log), Low-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Output per hour for low-skilled vs high-skilled workers
#======================================================================================================================#
sdid_output_perhr_lowskilled <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output.perhr",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perhr_lowskilled, digits.stats = 3, digits = 3)
etable(sdid_output_perhr_lowskilled, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_perhr_lowskilled, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_perhr_lowskilled, xlim = c(-3, 3), ylim = c(-0.8, 1.8), col = "blue",
	  main = "Output per hour (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_perhr_highskilled <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output.perhr",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perhr_highskilled, digits.stats = 3, digits = 3)
iplot(sdid_output_perhr_highskilled, xlim = c(-3, 3), ylim = c(-0.15, 0.55), col = "blue",
	  main = "Output per hour (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Output per hour for labour- v. capital-intensive industries
#======================================================================================================================#
sdid_output_perhr_li <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output.perhr",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perhr_li, digits.stats = 3, digits = 3)
etable(sdid_output_perhr_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_perhr_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_perhr_li, xlim = c(-3, 3), ylim = c(-0.4, 0.5), col = "blue",
	  main = "Output per hour (log), Labour-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_perhr_ci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output.perhr",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perhr_ci, digits.stats = 3, digits = 3)
iplot(sdid_output_perhr_ci, xlim = c(-3, 3), ylim = c(-0.25, 0.4), col = "blue",
	  main = "Output per hour (log), Capital-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Output per hour for high- v. low-profit industries
#======================================================================================================================#
sdid_output_perhr_hp <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output.perhr",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perhr_hp, digits.stats = 3, digits = 3)
etable(sdid_output_perhr_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_perhr_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_perhr_hp, xlim = c(-3, 3), ylim = c(-0.55, 0.7), col = "blue",
	  main = "Output per hour (log), High-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_perhr_lp <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output.perhr",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perhr_lp, digits.stats = 3, digits = 3)
iplot(sdid_output_perhr_lp, xlim = c(-3, 3), ylim = c(-0.05, 0.45), col = "blue",
	  main = "Output per hour (log), Low-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Output per worker for low-skilled vs high-skilled workers
#======================================================================================================================#
sdid_output_perworker_lowskilled <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output.perworker",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perworker_lowskilled, digits.stats = 3, digits = 3)
etable(sdid_output_perworker_lowskilled, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_perworker_lowskilled, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_perworker_lowskilled, xlim = c(-3, 3), ylim = c(-0.8, 1.8), col = "blue",
	  main = "Output per worker (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_perworker_highskilled <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output.perworker",
  interact_var = "low.skilled.workers",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perworker_highskilled, digits.stats = 3, digits = 3)
iplot(sdid_output_perworker_highskilled, xlim = c(-3, 3), ylim = c(-0.1, 0.45), col = "blue",
	  main = "Output per worker (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Output per worker for labour- v. capital-intensive industries
#======================================================================================================================#
sdid_output_perworker_li <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output.perworker",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perworker_li, digits.stats = 3, digits = 3)
etable(sdid_output_perworker_li, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_perworker_li, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_perworker_li, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Output per worker (log), Labour-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_perworker_ci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output.perworker",
  interact_var = "labour.intensive",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perworker_ci, digits.stats = 3, digits = 3)
iplot(sdid_output_perworker_ci, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Output per worker (log), Capital-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Output per worker for high- v. low-profit industries
#======================================================================================================================#
sdid_output_perworker_hp <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output.perworker",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perworker_hp, digits.stats = 3, digits = 3)
etable(sdid_output_perworker_hp, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_perworker_hp, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_perworker_hp, xlim = c(-3, 3), ylim = c(-0.65, 0.65), col = "blue",
	  main = "Output per worker (log), High-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_perworker_lp <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output.perworker",
  interact_var = "high.profit",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perworker_lp, digits.stats = 3, digits = 3)
iplot(sdid_output_perworker_lp, xlim = c(-3, 3), ylim = c(-0.05, 0.4), col = "blue",
	  main = "Output per worker (log), Low-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_output_heter.pdf", width = 24, height = 11)
par(mfrow = c(3, 6))
iplot(sdid_output_lowskilled, xlim = c(-3, 3), ylim = c(-2.5, 2.5), col = "blue",
	  main = "Output (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_highskilled, xlim = c(-3, 3), ylim = c(-0.25, 0.35), col = "blue",
	  main = "Output (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_li, xlim = c(-3, 3), ylim = c(-0.6, 0.7), col = "blue",
	  main = "Output (log), Labour-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_ci, xlim = c(-3, 3), ylim = c(-0.3, 0.5), col = "blue",
	  main = "Output (log), Capital-intensive ", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_hp, xlim = c(-3, 3), ylim = c(-0.3, 0.8), col = "blue",
	  main = "Output (log), High-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_lp, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Output (log), Low-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perhr_lowskilled, xlim = c(-3, 3), ylim = c(-0.8, 1.8), col = "blue",
	  main = "Output per hour (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perhr_highskilled, xlim = c(-3, 3), ylim = c(-0.15, 0.55), col = "blue",
	  main = "Output per hour (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perhr_li, xlim = c(-3, 3), ylim = c(-0.4, 0.5), col = "blue",
	  main = "Output per hour (log), Labour-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perhr_ci, xlim = c(-3, 3), ylim = c(-0.25, 0.4), col = "blue",
	  main = "Output per hour (log), Capital-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perhr_hp, xlim = c(-3, 3), ylim = c(-0.55, 0.7), col = "blue",
	  main = "Output per hour (log), High-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perhr_lp, xlim = c(-3, 3), ylim = c(-0.05, 0.45), col = "blue",
	  main = "Output per hour (log), Low-Profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perworker_lowskilled, xlim = c(-3, 3), ylim = c(-0.8, 1.8), col = "blue",
	  main = "Output per worker (log), Low-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perworker_highskilled, xlim = c(-3, 3), ylim = c(-0.1, 0.45), col = "blue",
	  main = "Output per worker (log), High-skilled Workers", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perworker_li, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
	  main = "Output per worker (log), Labour-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perworker_ci, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Output per worker (log), Capital-intensive", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perworker_hp, xlim = c(-3, 3), ylim = c(-0.65, 0.65), col = "blue",
	  main = "Output per worker (log), High-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perworker_lp, xlim = c(-3, 3), ylim = c(-0.05, 0.4), col = "blue",
	  main = "Output per worker (log), Low-profit", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Wage per hour for HPLI
#======================================================================================================================#
sdid_wages_hpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.wage.perhr",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_wages_hpli, digits.stats = 3, digits = 3)
etable(sdid_wages_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_wages_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_wages_hpli, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = "blue",
	  main = "Hourly wage (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_wages_hpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.wage.perhr",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_wages_hpci, digits.stats = 3, digits = 3)
iplot(sdid_wages_hpci, xlim = c(-3, 3), ylim = c(-0.05, 0.1), col = "blue",
	  main = "Hourly wage (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Wage per hour for LPLI
#======================================================================================================================#
sdid_wages_lpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.wage.perhr",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_wages_lpli, digits.stats = 3, digits = 3)
etable(sdid_wages_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_wages_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_wages_lpli, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
	  main = "Hourly wage (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_wages_lpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.wage.perhr",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_wages_lpci, digits.stats = 3, digits = 3)
iplot(sdid_wages_lpci, xlim = c(-3, 3), ylim = c(-0.05, 0.1), col = "blue",
	  main = "Hourly wage (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Labour cost: Industry Pay---Total payroll for HPLI
#======================================================================================================================#
sdid_pay_hpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.pay",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_pay_hpli, digits.stats = 3, digits = 3)
etable(sdid_pay_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_pay_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_pay_hpli, xlim = c(-3, 3), ylim = c(-1.7, 0.25), col = "blue",
	  main = "Total Payroll (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_pay_hpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.pay",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_pay_hpci, digits.stats = 3, digits = 3)
iplot(sdid_pay_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Payroll (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Labour cost: Industry Pay---Total payroll for LPLI
#======================================================================================================================#
sdid_pay_lpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.pay",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_pay_lpli, digits.stats = 3, digits = 3)
etable(sdid_pay_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_pay_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_pay_lpli, xlim = c(-3, 3), ylim = c(-0.3, 0.5), col = "blue",
	  main = "Total Payroll (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_pay_lpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.pay",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_pay_lpci, digits.stats = 3, digits = 3)
iplot(sdid_pay_lpci, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Payroll (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_industry_cost_heter_hpli_lpli.pdf", width = 18, height = 7)
par(mfrow = c(2, 4))
iplot(sdid_wages_hpli, xlim = c(-3, 3), ylim = c(-0.4, 0.2), col = "blue",
	  main = "Hourly wage (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_wages_hpci, xlim = c(-3, 3), ylim = c(-0.05, 0.1), col = "blue",
	  main = "Hourly wage (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_wages_lpli, xlim = c(-3, 3), ylim = c(-0.15, 0.1), col = "blue",
	  main = "Hourly wage (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_wages_lpci, xlim = c(-3, 3), ylim = c(-0.05, 0.1), col = "blue",
	  main = "Hourly wage (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_pay_hpli, xlim = c(-3, 3), ylim = c(-1.7, 0.25), col = "blue",
	  main = "Total Payroll (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_pay_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Total Payroll (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_pay_lpli, xlim = c(-3, 3), ylim = c(-0.3, 0.5), col = "blue",
	  main = "Total Payroll (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_pay_lpci, xlim = c(-3, 3), ylim = c(-0.2, 0.2), col = "blue",
	  main = "Total Payroll (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T,
	  ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Employment for HPLI
#======================================================================================================================#
sdid_emp_hpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.emp",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_emp_hpli, digits.stats = 3, digits = 3)
etable(sdid_emp_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_emp_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_emp_hpli, xlim = c(-3, 3), ylim = c(-1.4, 0.4), col = "blue",
	  main = "Employment (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_emp_hpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.emp",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_emp_hpci, digits.stats = 3, digits = 3)
iplot(sdid_emp_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Employment (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Employment for LPLI
#======================================================================================================================#
sdid_emp_lpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.emp",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_emp_lpli, digits.stats = 3, digits = 3)
etable(sdid_emp_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_emp_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_emp_lpli, xlim = c(-3, 3), ylim = c(-0.3, 0.7), col = "blue",
	  main = "Employment (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_emp_lpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.emp",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_emp_lpci, digits.stats = 3, digits = 3)
iplot(sdid_emp_lpci, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Employment (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Production workers for HPLI
#======================================================================================================================#
sdid_prodworkers_hpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.prodw",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodworkers_hpli, digits.stats = 3, digits = 3)
etable(sdid_prodworkers_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_prodworkers_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_prodworkers_hpli, xlim = c(-3, 3), ylim = c(-1.8, 0.4), col = "blue",
	  main = "Production Workers (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_prodworkers_hpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.prodw",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodworkers_hpci, digits.stats = 3, digits = 3)
iplot(sdid_prodworkers_hpci, xlim = c(-3, 3), ylim = c(-0.15, 0.4), col = "blue",
	  main = "Production Workers (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Production workers for LPLI
#======================================================================================================================#
sdid_prodworkers_lpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.prodw",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodworkers_lpli, digits.stats = 3, digits = 3)
etable(sdid_prodworkers_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_prodworkers_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_prodworkers_lpli, xlim = c(-3, 3), ylim = c(-0.25, 0.5), col = "blue",
	  main = "Production Workers (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_prodworkers_lpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.prodw",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodworkers_lpci, digits.stats = 3, digits = 3)
iplot(sdid_prodworkers_lpci, xlim = c(-3, 3), ylim = c(-0.35, 0.3), col = "blue",
	  main = "Production Workers (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Production Hours for HPLI
#======================================================================================================================#
sdid_prodhours_hpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.prodh",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodhours_hpli, digits.stats = 3, digits = 3)
etable(sdid_prodhours_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_prodhours_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_prodhours_hpli, xlim = c(-3, 3), ylim = c(-1.7, 0.2), col = "blue",
	  main = "Production Hours (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_prodhours_hpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.prodh",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodhours_hpci, digits.stats = 3, digits = 3)
iplot(sdid_prodhours_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Production Hours (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Production Hours for LPLI
#======================================================================================================================#
sdid_prodhours_lpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.prodh",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodhours_lpli, digits.stats = 3, digits = 3)
etable(sdid_prodhours_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_prodhours_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_prodhours_lpli, xlim = c(-3, 3), ylim = c(-0.25, 0.6), col = "blue",
	  main = "Production Hours (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_prodhours_lpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.prodh",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_prodhours_lpci, digits.stats = 3, digits = 3)
iplot(sdid_prodhours_lpci, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
	  main = "Production Hours (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_emp_hours_heter_hpli_lpli.pdf", width = 24, height = 7)
par(mfrow = c(2, 6))
iplot(sdid_emp_hpli, xlim = c(-3, 3), ylim = c(-1.4, 0.4), col = "blue",
	  main = "Employment (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_emp_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Employment (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_emp_lpli, xlim = c(-3, 3), ylim = c(-0.3, 0.7), col = "blue",
	  main = "Employment (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_emp_lpci, xlim = c(-3, 3), ylim = c(-0.3, 0.2), col = "blue",
	  main = "Employment (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodworkers_hpli, xlim = c(-3, 3), ylim = c(-1.8, 0.4), col = "blue",
	  main = "Production Workers (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodworkers_hpci, xlim = c(-3, 3), ylim = c(-0.15, 0.4), col = "blue",
	  main = "Production Workers (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodworkers_lpli, xlim = c(-3, 3), ylim = c(-0.25, 0.5), col = "blue",
	  main = "Production Workers (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodworkers_lpci, xlim = c(-3, 3), ylim = c(-0.35, 0.3), col = "blue",
	  main = "Production Workers (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodhours_hpli, xlim = c(-3, 3), ylim = c(-1.7, 0.2), col = "blue",
	  main = "Production Hours (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodhours_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.4), col = "blue",
	  main = "Production Hours (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodhours_lpli, xlim = c(-3, 3), ylim = c(-0.25, 0.6), col = "blue",
	  main = "Production Hours (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_prodhours_lpci, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
	  main = "Production Hours (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#
### Output for HPLI
#======================================================================================================================#
sdid_output_hpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_hpli, digits.stats = 3, digits = 3)
etable(sdid_output_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_hpli, xlim = c(-3, 3), ylim = c(-1.4, 0.55), col = "blue",
	  main = "Output (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_hpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_hpci, digits.stats = 3, digits = 3)
iplot(sdid_output_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Output (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Output for LPLI
#======================================================================================================================#
sdid_output_lpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_lpli, digits.stats = 3, digits = 3)
etable(sdid_output_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_lpli, xlim = c(-3, 3), ylim = c(-0.6, 0.4), col = "blue",
	  main = "Output (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_lpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_lpci, digits.stats = 3, digits = 3)
iplot(sdid_output_lpci, xlim = c(-3, 3), ylim = c(-0.25, 0.4), col = "blue",
	  main = "Output (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Output per hour for HPLI
#======================================================================================================================#
sdid_output_perhr_hpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output.perhr",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perhr_hpli, digits.stats = 3, digits = 3)
etable(sdid_output_perhr_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_perhr_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_perhr_hpli, xlim = c(-3, 3), ylim = c(-0.6, 0.7), col = "blue",
	  main = "Output per hour (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_perhr_hpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output.perhr",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perhr_hpci, digits.stats = 3, digits = 3)
iplot(sdid_output_perhr_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.25), col = "blue",
	  main = "Output per hour (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Output per hour for LPLI
#======================================================================================================================#
sdid_output_perhr_lpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output.perhr",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perhr_lpli, digits.stats = 3, digits = 3)
etable(sdid_output_perhr_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_perhr_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_perhr_lpli, xlim = c(-3, 3), ylim = c(-0.75, 0.25), col = "blue",
	  main = "Output per hour (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_perhr_lpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output.perhr",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perhr_lpci, digits.stats = 3, digits = 3)
iplot(sdid_output_perhr_lpci, xlim = c(-3, 3), ylim = c(-0.2, 0.55), col = "blue",
	  main = "Output per hour (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Output per worker for HPLI
#======================================================================================================================#
sdid_output_perworker_hpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output.perworker",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perworker_hpli, digits.stats = 3, digits = 3)
etable(sdid_output_perworker_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_perworker_hpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_perworker_hpli, xlim = c(-3, 3), ylim = c(-0.6, 0.7), col = "blue",
	  main = "Output per worker (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_perworker_hpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output.perworker",
  interact_var = "high.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perworker_hpci, digits.stats = 3, digits = 3)
iplot(sdid_output_perworker_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.25), col = "blue",
	  main = "Output per worker (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
### Output per worker for LPLI
#======================================================================================================================#
sdid_output_perworker_lpli <- sdid_baseline_heter_1(
  data = triQc,
  depvar = "l.output.perworker",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perworker_lpli, digits.stats = 3, digits = 3)
etable(sdid_output_perworker_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_output_perworker_lpli, agg = "cohort", digits = 3, digits.stats = 3)
iplot(sdid_output_perworker_lpli, xlim = c(-3, 3), ylim = c(-0.7, 0.2), col = "blue",
	  main = "Output per worker (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_output_perworker_lpci <- sdid_baseline_heter_0(
  data = triQc,
  depvar = "l.output.perworker",
  interact_var = "low.profit.labour",
  cluster = ~facility.state,
  fes = county_fes()
)
etable(sdid_output_perworker_lpci, digits.stats = 3, digits = 3)
iplot(sdid_output_perworker_lpci, xlim = c(-3, 3), ylim = c(-0.15, 0.5), col = "blue",
	  main = "Output per worker (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_sdid_output_heter_hpli_lpli.pdf", width = 24, height = 7)
par(mfrow = c(2, 6))
iplot(sdid_output_hpli, xlim = c(-3, 3), ylim = c(-1.4, 0.55), col = "blue",
	  main = "Output (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.3), col = "blue",
	  main = "Output (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_lpli, xlim = c(-3, 3), ylim = c(-0.6, 0.4), col = "blue",
	  main = "Output (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_lpci, xlim = c(-3, 3), ylim = c(-0.25, 0.4), col = "blue",
	  main = "Output (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perhr_hpli, xlim = c(-3, 3), ylim = c(-0.6, 0.7), col = "blue",
	  main = "Output per hour (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perhr_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.25), col = "blue",
	  main = "Output per hour (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perhr_lpli, xlim = c(-3, 3), ylim = c(-0.75, 0.25), col = "blue",
	  main = "Output per hour (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perhr_lpci, xlim = c(-3, 3), ylim = c(-0.2, 0.55), col = "blue",
	  main = "Output per hour (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perworker_hpli, xlim = c(-3, 3), ylim = c(-0.6, 0.7), col = "blue",
	  main = "Output per worker (log), HPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perworker_hpci, xlim = c(-3, 3), ylim = c(-0.2, 0.25), col = "blue",
	  main = "Output per worker (log), HPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perworker_lpli, xlim = c(-3, 3), ylim = c(-0.7, 0.2), col = "blue",
	  main = "Output per worker (log), LPLI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
iplot(sdid_output_perworker_lpci, xlim = c(-3, 3), ylim = c(-0.15, 0.5), col = "blue",
	  main = "Output per worker (log), LPCI", xlab = "relative year", lwd = 1, cex = 4,
	  pt.cex = 1.5, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1
) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#