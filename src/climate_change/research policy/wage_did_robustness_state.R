#======================================================================================================================#
### Baseline Robustness Checks
#======================================================================================================================#
### Packages
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
#======================================================================================================================#
### Loading Data
#======================================================================================================================#
source(file = "your_path/functions.R", echo = T)
file <- "your_path/triQc_onsite_econj.rds"
triQs <- read_rds(file = file)
#======================================================================================================================#
### Alternative clustering of the SEs: at naics code and facility ID levels
#======================================================================================================================#
### Hourly Wage
#======================================================================================================================#
reg_wagephr_naics <- sdid_baseline(
  data = triQs,
  depvar = "wage.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = state_fes()
)
etable(reg_wagephr_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_wagephr_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_wagephr_fac_id <- sdid_baseline(
  data = triQs,
  depvar = "wage.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = state_fes()
)
etable(reg_wagephr_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_wagephr_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Labour cost: Industry Pay---Total payroll
#======================================================================================================================#
reg_pay_naics <- sdid_baseline(
  data = triQs,
  depvar = "l.pay",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = state_fes()
)
etable(reg_pay_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_pay_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_pay_fac_id <- sdid_baseline(
  data = triQs,
  depvar = "l.pay",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = state_fes()
)
etable(reg_pay_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_pay_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material cost: Industry material cost
#======================================================================================================================#
reg_matcost_naics <- sdid_baseline(
  data = triQs,
  depvar = "l.matcost",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = state_fes()
)
etable(reg_matcost_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_matcost_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_matcost_fac_id <- sdid_baseline(
  data = triQs,
  depvar = "l.matcost",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = state_fes()
)
etable(reg_matcost_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_matcost_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Employment
#======================================================================================================================#
reg_emp_naics <- sdid_baseline(
  data = triQs,
  depvar = "l.emp",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = state_fes()
)
etable(reg_emp_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_emp_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_emp_fac_id <- sdid_baseline(
  data = triQs,
  depvar = "l.emp",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = state_fes()
)
etable(reg_emp_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_emp_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Production workers
#======================================================================================================================#
reg_pworkers_naics <- sdid_baseline(
  data = triQs,
  depvar = "l.prode",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = state_fes()
)
etable(reg_pworkers_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_pworkers_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_pworkers_fac_id <- sdid_baseline(
  data = triQs,
  depvar = "l.prode",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = state_fes()
)
etable(reg_pworkers_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_pworkers_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Production hours
#======================================================================================================================#
reg_phours_naics <- sdid_baseline(
  data = triQs,
  depvar = "l.prodh",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = state_fes()
)
etable(reg_phours_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_phours_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_phours_fac_id <- sdid_baseline(
  data = triQs,
  depvar = "l.prodh",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = state_fes()
)
etable(reg_phours_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_phours_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Industry Output
#======================================================================================================================#
reg_output_naics <- sdid_baseline(
  data = triQs,
  depvar = "l.output",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = state_fes()
)
etable(reg_output_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_output_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_output_fac_id <- sdid_baseline(
  data = triQs,
  depvar = "l.output",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = state_fes()
)
etable(reg_output_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_output_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Output per hour
#======================================================================================================================#
reg_outputprhr_naics <- sdid_baseline(
  data = triQs,
  depvar = "l.output.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = state_fes()
)
etable(reg_outputprhr_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_outputprhr_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_outputprhr_fac_id <- sdid_baseline(
  data = triQs,
  depvar = "l.output.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = state_fes()
)
etable(reg_outputprhr_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_outputprhr_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Output per Worker
#======================================================================================================================#
reg_outputperworker_naics <- sdid_baseline(
  data = triQs,
  depvar = "l.output.perworker",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = state_fes()
)
etable(reg_outputperworker_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_outputperworker_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_outputperworker_fac_id <- sdid_baseline(
  data = triQs,
  depvar = "l.output.perworker",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = state_fes()
)
etable(reg_outputperworker_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_outputperworker_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Cross-County Mobility
#======================================================================================================================#
### Industry: Employment
#======================================================================================================================#
reg_emp_mob <- sdid_baseline_heter_mobility(
  data = triQs,
  depvar = "l.emp",
  interact_var = "dist.to.border",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(reg_emp_mob, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_emp_mob, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Production workers
#======================================================================================================================#
reg_prodworkers_mob <- sdid_baseline_heter_mobility(
  data = triQs,
  depvar = "l.prode",
  interact_var = "dist.to.border",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(reg_prodworkers_mob, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_prodworkers_mob, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Production Hours
#======================================================================================================================#
reg_prodhours_mob <- sdid_baseline_heter_mobility(
  data = triQs,
  depvar = "l.prodh",
  interact_var = "dist.to.border",
  cluster = ~facility.state,
  fes = did_state_fes()
)
etable(reg_prodhours_mob, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_prodhours_mob, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#