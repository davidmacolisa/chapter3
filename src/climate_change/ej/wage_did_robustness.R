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
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading Data
#======================================================================================================================#
source(file = "./Thesis/chapter3/src/climate_change/codes/functions.R", echo = T)
file <- "./Data_PhD/US/BLS/onsite/triQc_on.rds"
triQc <- read_rds(file = file)
#======================================================================================================================#
### Alternative clustering of the SEs
#======================================================================================================================#
### Hourly Wage
#======================================================================================================================#
reg_wagephr_fips <- sdid_baseline(
  data = triQc,
  depvar = "wage.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~fips.code.fe, # fips.code.fe
  fes = county_fes()
)
etable(reg_wagephr_fips, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_wagephr_fips, agg = "cohort", digits = 3, digits.stats = 3)

reg_wagephr_naics <- sdid_baseline(
  data = triQc,
  depvar = "wage.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = county_fes()
)
etable(reg_wagephr_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_wagephr_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_wagephr_zipcode <- sdid_baseline(
  data = triQc,
  depvar = "wage.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.zipcode, # facility.zipcode
  fes = county_fes()
)
etable(reg_wagephr_zipcode, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_wagephr_zipcode, agg = "cohort", digits = 3, digits.stats = 3)

reg_wagephr_fac_id <- sdid_baseline(
  data = triQc,
  depvar = "wage.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = county_fes()
)
etable(reg_wagephr_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_wagephr_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Labour cost: Industry Pay---Total payroll
#======================================================================================================================#
reg_pay_fips <- sdid_baseline(
  data = triQc,
  depvar = "l.pay",
  ATT = "sunab(ch.year, year)",
  cluster = ~fips.code.fe, # fips.code.fe
  fes = county_fes()
)
etable(reg_pay_fips, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_pay_fips, agg = "cohort", digits = 3, digits.stats = 3)

reg_pay_naics <- sdid_baseline(
  data = triQc,
  depvar = "l.pay",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = county_fes()
)
etable(reg_pay_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_pay_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_pay_zipcode <- sdid_baseline(
  data = triQc,
  depvar = "l.pay",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.zipcode, # facility.zipcode
  fes = county_fes()
)
etable(reg_pay_zipcode, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_pay_zipcode, agg = "cohort", digits = 3, digits.stats = 3)

reg_pay_fac_id <- sdid_baseline(
  data = triQc,
  depvar = "l.pay",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = county_fes()
)
etable(reg_pay_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_pay_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material cost: Industry material cost
#======================================================================================================================#
reg_matcost_fips <- sdid_baseline(
  data = triQc,
  depvar = "l.matcost",
  ATT = "sunab(ch.year, year)",
  cluster = ~fips.code.fe, # fips.code.fe
  fes = county_fes()
)
etable(reg_matcost_fips, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_matcost_fips, agg = "cohort", digits = 3, digits.stats = 3)

reg_matcost_naics <- sdid_baseline(
  data = triQc,
  depvar = "l.matcost",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = county_fes()
)
etable(reg_matcost_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_matcost_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_matcost_zipcode <- sdid_baseline(
  data = triQc,
  depvar = "l.matcost",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.zipcode, # facility.zipcode
  fes = county_fes()
)
etable(reg_matcost_zipcode, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_matcost_zipcode, agg = "cohort", digits = 3, digits.stats = 3)

reg_matcost_fac_id <- sdid_baseline(
  data = triQc,
  depvar = "l.matcost",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = county_fes()
)
etable(reg_matcost_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_matcost_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Employment
#======================================================================================================================#
reg_emp_fips <- sdid_baseline(
  data = triQc,
  depvar = "l.emp",
  ATT = "sunab(ch.year, year)",
  cluster = ~fips.code.fe, # fips.code.fe
  fes = county_fes()
)
etable(reg_emp_fips, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_emp_fips, agg = "cohort", digits = 3, digits.stats = 3)

reg_emp_naics <- sdid_baseline(
  data = triQc,
  depvar = "l.emp",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = county_fes()
)
etable(reg_emp_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_emp_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_emp_zipcode <- sdid_baseline(
  data = triQc,
  depvar = "l.emp",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.zipcode, # facility.zipcode
  fes = county_fes()
)
etable(reg_emp_zipcode, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_emp_zipcode, agg = "cohort", digits = 3, digits.stats = 3)

reg_emp_fac_id <- sdid_baseline(
  data = triQc,
  depvar = "l.emp",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = county_fes()
)
etable(reg_emp_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_emp_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Production workers
#======================================================================================================================#
reg_pworkers_fips <- sdid_baseline(
  data = triQc,
  depvar = "l.prode",
  ATT = "sunab(ch.year, year)",
  cluster = ~fips.code.fe, # fips.code.fe
  fes = county_fes()
)
etable(reg_pworkers_fips, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_pworkers_fips, agg = "cohort", digits = 3, digits.stats = 3)

reg_pworkers_naics <- sdid_baseline(
  data = triQc,
  depvar = "l.prode",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = county_fes()
)
etable(reg_pworkers_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_pworkers_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_pworkers_zipcode <- sdid_baseline(
  data = triQc,
  depvar = "l.prode",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.zipcode, # facility.zipcode
  fes = county_fes()
)
etable(reg_pworkers_zipcode, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_pworkers_zipcode, agg = "cohort", digits = 3, digits.stats = 3)

reg_pworkers_fac_id <- sdid_baseline(
  data = triQc,
  depvar = "l.prode",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = county_fes()
)
etable(reg_pworkers_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_pworkers_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Production hours
#======================================================================================================================#
reg_phours_fips <- sdid_baseline(
  data = triQc,
  depvar = "l.prodh",
  ATT = "sunab(ch.year, year)",
  cluster = ~fips.code.fe, # fips.code.fe
  fes = county_fes()
)
etable(reg_phours_fips, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_phours_fips, agg = "cohort", digits = 3, digits.stats = 3)

reg_phours_naics <- sdid_baseline(
  data = triQc,
  depvar = "l.prodh",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = county_fes()
)
etable(reg_phours_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_phours_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_phours_zipcode <- sdid_baseline(
  data = triQc,
  depvar = "l.prodh",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.zipcode, # facility.zipcode
  fes = county_fes()
)
etable(reg_phours_zipcode, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_phours_zipcode, agg = "cohort", digits = 3, digits.stats = 3)

reg_phours_fac_id <- sdid_baseline(
  data = triQc,
  depvar = "l.prodh",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = county_fes()
)
etable(reg_phours_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_phours_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Industry Output
#======================================================================================================================#
reg_output_fips <- sdid_baseline(
  data = triQc,
  depvar = "l.output",
  ATT = "sunab(ch.year, year)",
  cluster = ~fips.code.fe, # fips.code.fe
  fes = county_fes()
)
etable(reg_output_fips, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_output_fips, agg = "cohort", digits = 3, digits.stats = 3)

reg_output_naics <- sdid_baseline(
  data = triQc,
  depvar = "l.output",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = county_fes()
)
etable(reg_output_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_output_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_output_zipcode <- sdid_baseline(
  data = triQc,
  depvar = "l.output",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.zipcode, # facility.zipcode
  fes = county_fes()
)
etable(reg_output_zipcode, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_output_zipcode, agg = "cohort", digits = 3, digits.stats = 3)

reg_output_fac_id <- sdid_baseline(
  data = triQc,
  depvar = "l.output",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = county_fes()
)
etable(reg_output_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_output_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Output per hour
#======================================================================================================================#
reg_outputprhr_fips <- sdid_baseline(
  data = triQc,
  depvar = "l.output.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~fips.code.fe, # fips.code.fe
  fes = county_fes()
)
etable(reg_outputprhr_fips, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_outputprhr_fips, agg = "cohort", digits = 3, digits.stats = 3)

reg_outputprhr_naics <- sdid_baseline(
  data = triQc,
  depvar = "l.output.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = county_fes()
)
etable(reg_outputprhr_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_outputprhr_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_outputprhr_zipcode <- sdid_baseline(
  data = triQc,
  depvar = "l.output.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.zipcode, # facility.zipcode
  fes = county_fes()
)
etable(reg_outputprhr_zipcode, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_outputprhr_zipcode, agg = "cohort", digits = 3, digits.stats = 3)

reg_outputprhr_fac_id <- sdid_baseline(
  data = triQc,
  depvar = "l.output.perhr",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = county_fes()
)
etable(reg_outputprhr_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_outputprhr_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Output per Worker
#======================================================================================================================#
reg_outputperworker_fips <- sdid_baseline(
  data = triQc,
  depvar = "l.output.perworker",
  ATT = "sunab(ch.year, year)",
  cluster = ~fips.code.fe, # fips.code.fe
  fes = county_fes()
)
etable(reg_outputperworker_fips, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_outputperworker_fips, agg = "cohort", digits = 3, digits.stats = 3)

reg_outputperworker_naics <- sdid_baseline(
  data = triQc,
  depvar = "l.output.perworker",
  ATT = "sunab(ch.year, year)",
  cluster = ~naics.code, # naics.code
  fes = county_fes()
)
etable(reg_outputperworker_naics, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_outputperworker_naics, agg = "cohort", digits = 3, digits.stats = 3)

reg_outputperworker_zipcode <- sdid_baseline(
  data = triQc,
  depvar = "l.output.perworker",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.zipcode, # facility.zipcode
  fes = county_fes()
)
etable(reg_outputperworker_zipcode, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_outputperworker_zipcode, agg = "cohort", digits = 3, digits.stats = 3)

reg_outputperworker_fac_id <- sdid_baseline(
  data = triQc,
  depvar = "l.output.perworker",
  ATT = "sunab(ch.year, year)",
  cluster = ~facility.id, # facility.id
  fes = county_fes()
)
etable(reg_outputperworker_fac_id, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_outputperworker_fac_id, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Cross-County Mobility
#======================================================================================================================#
### Industry: Employment
#======================================================================================================================#
reg_emp_mob <- sdid_baseline_heter_mobility(
  data = triQc,
  depvar = "l.emp",
  interact_var = "dist.to.border",
  cluster = ~facility.state,
  fes = did_county_fes()
)
etable(reg_emp_mob, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_emp_mob, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Production workers
#======================================================================================================================#
reg_prodworkers_mob <- sdid_baseline_heter_mobility(
  data = triQc,
  depvar = "l.prode",
  interact_var = "dist.to.border",
  cluster = ~facility.state,
  fes = did_county_fes()
)
etable(reg_prodworkers_mob, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_prodworkers_mob, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Production Hours
#======================================================================================================================#
reg_prodhours_mob <- sdid_baseline_heter_mobility(
  data = triQc,
  depvar = "l.prodh",
  interact_var = "dist.to.border",
  cluster = ~facility.state,
  fes = did_county_fes()
)
etable(reg_prodhours_mob, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_prodhours_mob, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#