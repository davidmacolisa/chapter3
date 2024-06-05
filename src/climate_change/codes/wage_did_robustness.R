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
  wage.perhr ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.county, # facility.county
)
fixest::etable(reg_wagephr, digits = 4, digits.stats = 4)

reg_wagephr <- fixest::feols(
  wage.perhr ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_wagephr, digits = 4, digits.stats = 4)

reg_wagephr <- fixest::feols(
  wage.perhr ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_wagephr, digits = 4, digits.stats = 4)

reg_wagephr <- fixest::feols(
  wage.perhr ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_wagephr, digits = 4, digits.stats = 4)

reg_wagephr <- fixest::feols(
  wage.perhr ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_wagephr, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Labour cost: Industry Pay---Total payroll
#======================================================================================================================#
reg_pay <- fixest::feols(
  pay ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.county, # facility.county
)
fixest::etable(reg_pay, digits = 4, digits.stats = 4)

reg_pay <- fixest::feols(
  pay ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_pay, digits = 4, digits.stats = 4)

reg_pay <- fixest::feols(
  pay ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_pay, digits = 4, digits.stats = 4)

reg_pay <- fixest::feols(
  pay ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_pay, digits = 4, digits.stats = 4)

reg_pay <- fixest::feols(
  pay ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_pay, digits = 4, digits.stats = 4)

#======================================================================================================================#
### Material cost: Industry material cost
#======================================================================================================================#
reg_matcost <- fixest::feols(
  l.matcost ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.county, # facility.county
)
fixest::etable(reg_matcost, digits = 4, digits.stats = 4)

reg_matcost <- fixest::feols(
  l.matcost ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_matcost, digits = 4, digits.stats = 4)

reg_matcost <- fixest::feols(
  l.matcost ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_matcost, digits = 4, digits.stats = 4)

reg_matcost <- fixest::feols(
  l.matcost ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_matcost, digits = 4, digits.stats = 4)

reg_matcost <- fixest::feols(
  l.matcost ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_matcost, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Industry: Employment
#======================================================================================================================#
reg_emp <- fixest::feols(
  l.emp ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.county, # facility.county
)
fixest::etable(reg_emp, digits = 4, digits.stats = 4)

reg_emp <- fixest::feols(
  l.emp ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_emp, digits = 4, digits.stats = 4)

reg_emp <- fixest::feols(
  l.emp ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_emp, digits = 4, digits.stats = 4)

reg_emp <- fixest::feols(
  l.emp ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_emp, digits = 4, digits.stats = 4)

reg_emp <- fixest::feols(
  l.emp ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_emp, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Industry: Production hours
#======================================================================================================================#
reg_phours <- fixest::feols(
  l.prodh ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.county,
)
fixest::etable(reg_phours, digits = 4, digits.stats = 4)

reg_phours <- fixest::feols(
  l.prodh ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_phours, digits = 4, digits.stats = 4)

reg_phours <- fixest::feols(
  l.prodh ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_phours, digits = 4, digits.stats = 4)

reg_phours <- fixest::feols(
  l.prodh ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_phours, digits = 4, digits.stats = 4)

reg_phours <- fixest::feols(
  l.prodh ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_phours, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Industry: Industry Output
#======================================================================================================================#
reg_output <- fixest::feols(
  log(vadd) ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.county,
)
fixest::etable(reg_output, digits = 4, digits.stats = 4)

reg_output <- fixest::feols(
  log(vadd) ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_output, digits = 4, digits.stats = 4)

reg_output <- fixest::feols(
  log(vadd) ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_output, digits = 4, digits.stats = 4)

reg_output <- fixest::feols(
  log(vadd) ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_output, digits = 4, digits.stats = 4)

reg_output <- fixest::feols(
  log(vadd) ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_output, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Industry: Output per hour
#======================================================================================================================#
reg_outputprhr <- fixest::feols(
  l.output.perhr ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.county,
)

fixest::etable(reg_outputprhr, digits = 4, digits.stats = 4)

reg_outputprhr <- fixest::feols(
  l.output.perhr ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~fips.code.fe, # fips.code.fe
)

fixest::etable(reg_outputprhr, digits = 4, digits.stats = 4)

reg_outputprhr <- fixest::feols(
  l.output.perhr ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~naics.code, # naics.code
)

fixest::etable(reg_outputprhr, digits = 4, digits.stats = 4)

reg_outputprhr <- fixest::feols(
  l.output.perhr ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.zipcode, # facility.zipcode
)

fixest::etable(reg_outputprhr, digits = 4, digits.stats = 4)

reg_outputprhr <- fixest::feols(
  l.output.perhr ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.id, # facility.id
)

fixest::etable(reg_outputprhr, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Industry: Output per Worker
#======================================================================================================================#
reg_outputperworker <- fixest::feols(
  l.output.perworker ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.county,
)
fixest::etable(reg_outputperworker, digits = 4, digits.stats = 4)

reg_outputperworker <- fixest::feols(
  l.output.perworker ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~fips.code.fe, # fips.code.fe
)
fixest::etable(reg_outputperworker, digits = 4, digits.stats = 4)

reg_outputperworker <- fixest::feols(
  l.output.perworker ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~naics.code, # naics.code
)

fixest::etable(reg_outputperworker, digits = 4, digits.stats = 4)

reg_outputperworker <- fixest::feols(
  l.output.perworker ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.zipcode, # facility.zipcode
)

fixest::etable(reg_outputperworker, digits = 4, digits.stats = 4)

reg_outputperworker <- fixest::feols(
  l.output.perworker ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
        population.1 +
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
  cluster = ~facility.id, # facility.id
)

fixest::etable(reg_outputperworker, digits = 4, digits.stats = 4)
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
    csw(,
      year,
      fips.code.fe,
      border.county.fe,
      border.county.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state
)
etable(reg_emp, agg = "ATT", digits = 4, digits.stats = 4)
etable(reg_emp, agg = "cohort", digits = 4, digits.stats = 4)
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
    csw(,
      year,
      fips.code.fe,
      border.county.fe,
      border.county.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state
)
etable(reg_prodworkers, agg = "ATT", digits = 4, digits.stats = 4)
etable(reg_prodworkers, agg = "cohort", digits = 4, digits.stats = 4)
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
    csw(,
      year,
      fips.code.fe,
      border.county.fe,
      border.county.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state
)
etable(reg_prodhours, agg = "ATT", digits = 4, digits.stats = 4)
etable(reg_prodhours, agg = "cohort", digits = 4, digits.stats = 4)
#======================================================================================================================#