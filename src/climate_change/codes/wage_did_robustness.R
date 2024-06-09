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
file <- "./Data_PhD/US/BLS/onsite/triQc_on.rds"
triQc <- read_rds(file = file)
#======================================================================================================================#
### Alternative clustering of the SEs
#======================================================================================================================#
### Hourly Wage
#======================================================================================================================#
reg_wagephr <- fixest::feols(
  wage.perhr ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_wagephr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_wagephr, agg = "cohort", digits = 3, digits.stats = 3)

reg_wagephr <- fixest::feols(
  wage.perhr ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_wagephr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_wagephr, agg = "cohort", digits = 3, digits.stats = 3)

reg_wagephr <- fixest::feols(
  wage.perhr ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_wagephr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_wagephr, agg = "cohort", digits = 3, digits.stats = 3)

reg_wagephr <- fixest::feols(
  wage.perhr ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_wagephr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_wagephr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Labour cost: Industry Pay---Total payroll
#======================================================================================================================#
reg_pay <- fixest::feols(
  l.pay ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_pay, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_pay, agg = "cohort", digits = 3, digits.stats = 3)

reg_pay <- fixest::feols(
  l.pay ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_pay, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_pay, agg = "cohort", digits = 3, digits.stats = 3)

reg_pay <- fixest::feols(
  l.pay ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_pay, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_pay, agg = "cohort", digits = 3, digits.stats = 3)

reg_pay <- fixest::feols(
  l.pay ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_pay, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_pay, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material cost: Industry material cost
#======================================================================================================================#
reg_matcost <- fixest::feols(
  l.matcost ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_matcost, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_matcost, agg = "cohort", digits = 3, digits.stats = 3)

reg_matcost <- fixest::feols(
  l.matcost ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_matcost, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_matcost, agg = "cohort", digits = 3, digits.stats = 3)

reg_matcost <- fixest::feols(
  l.matcost ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_matcost, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_matcost, agg = "cohort", digits = 3, digits.stats = 3)

reg_matcost <- fixest::feols(
  l.matcost ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_matcost, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_matcost, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Employment
#======================================================================================================================#
reg_emp <- fixest::feols(
  l.emp ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_emp, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_emp, agg = "cohort", digits = 3, digits.stats = 3)

reg_emp <- fixest::feols(
  l.emp ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_emp, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_emp, agg = "cohort", digits = 3, digits.stats = 3)

reg_emp <- fixest::feols(
  l.emp ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_emp, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_emp, agg = "cohort", digits = 3, digits.stats = 3)

reg_emp <- fixest::feols(
  l.emp ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_emp, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_emp, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Production workers
#======================================================================================================================#
reg_pworkers <- fixest::feols(
  l.prode ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_pworkers, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_pworkers, agg = "cohort", digits = 3, digits.stats = 3)

reg_pworkers <- fixest::feols(
  l.prode ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_pworkers, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_pworkers, agg = "cohort", digits = 3, digits.stats = 3)

reg_pworkers <- fixest::feols(
  l.prode ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_pworkers, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_pworkers, agg = "cohort", digits = 3, digits.stats = 3)

reg_pworkers <- fixest::feols(
  l.prode ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_pworkers, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_pworkers, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Production hours
#======================================================================================================================#
reg_phours <- fixest::feols(
  l.prodh ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_phours, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_phours, agg = "cohort", digits = 3, digits.stats = 3)

reg_phours <- fixest::feols(
  l.prodh ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_phours, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_phours, agg = "cohort", digits = 3, digits.stats = 3)

reg_phours <- fixest::feols(
  l.prodh ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_phours, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_phours, agg = "cohort", digits = 3, digits.stats = 3)

reg_phours <- fixest::feols(
  l.prodh ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_phours, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_phours, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Industry Output
#======================================================================================================================#
reg_output <- fixest::feols(
  log(vadd) ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_output, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_output, agg = "cohort", digits = 3, digits.stats = 3)

reg_output <- fixest::feols(
  log(vadd) ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_output, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_output, agg = "cohort", digits = 3, digits.stats = 3)

reg_output <- fixest::feols(
  log(vadd) ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_output, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_output, agg = "cohort", digits = 3, digits.stats = 3)

reg_output <- fixest::feols(
  log(vadd) ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_output, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_output, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Output per hour
#======================================================================================================================#
reg_outputprhr <- fixest::feols(
  l.output.perhr ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_outputprhr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_outputprhr, agg = "cohort", digits = 3, digits.stats = 3)

reg_outputprhr <- fixest::feols(
  l.output.perhr ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_outputprhr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_outputprhr, agg = "cohort", digits = 3, digits.stats = 3)

reg_outputprhr <- fixest::feols(
  l.output.perhr ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_outputprhr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_outputprhr, agg = "cohort", digits = 3, digits.stats = 3)

reg_outputprhr <- fixest::feols(
  l.output.perhr ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_outputprhr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_outputprhr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Output per Worker
#======================================================================================================================#
reg_outputperworker <- fixest::feols(
  l.output.perworker ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_outputperworker, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_outputperworker, agg = "cohort", digits = 3, digits.stats = 3)

reg_outputperworker <- fixest::feols(
  l.output.perworker ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_outputperworker, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_outputperworker, agg = "cohort", digits = 3, digits.stats = 3)

reg_outputperworker <- fixest::feols(
  l.output.perworker ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_outputperworker, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_outputperworker, agg = "cohort", digits = 3, digits.stats = 3)

reg_outputperworker <- fixest::feols(
  l.output.perworker ~ sunab(ch.year, year) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_outputperworker, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(reg_outputperworker, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Cross-County Mobility
#======================================================================================================================#
### Industry: Employment
#======================================================================================================================#
reg_emp <- fixest::feols(
  l.emp ~ sunab(ch.year, year):dist.to.border +
    e.treated +
    dist.to.border +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.state
)
etable(reg_emp, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_emp, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Production workers
#======================================================================================================================#
reg_prodworkers <- fixest::feols(
  l.prode ~ sunab(ch.year, year):dist.to.border +
    e.treated +
    dist.to.border +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.state
)
etable(reg_prodworkers, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_prodworkers, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Industry: Production Hours
#======================================================================================================================#
reg_prodhours <- fixest::feols(
  l.prodh ~ sunab(ch.year, year):dist.to.border +
    e.treated +
    dist.to.border +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      fips.code.fe +
      border.county.fe +
      border.county.year.fe
  ,
  data = triQc,
  cluster = ~facility.state
)
etable(reg_prodhours, agg = "ATT", digits = 3, digits.stats = 3)
etable(reg_prodhours, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#