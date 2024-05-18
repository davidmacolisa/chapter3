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
### Labour cost: Industry production workers' wages
#======================================================================================================================#
reg_wage <- fixest::feols(
  prodw ~ e.treated +
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.county, # facility.county
)
fixest::etable(reg_wage, digits = 4, digits.stats = 4)

reg_wage <- fixest::feols(
  prodw ~ e.treated +
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~fips.code, # fips.code
)
fixest::etable(reg_wage, digits = 4, digits.stats = 4)

reg_wage <- fixest::feols(
  prodw ~ e.treated +
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_wage, digits = 4, digits.stats = 4)

reg_wage <- fixest::feols(
  prodw ~ e.treated +
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_wage, digits = 4, digits.stats = 4)

reg_wage <- fixest::feols(
  prodw ~ e.treated +
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_wage, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Labour cost: Industry production wages per worker
#======================================================================================================================#
reg_wage_pw <- fixest::feols(
  wage.perworker ~ e.treated +
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.county, # facility.county
)
fixest::etable(reg_wage_pw, digits = 4, digits.stats = 4)

reg_wage_pw <- fixest::feols(
  wage.perworker ~ e.treated +
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~fips.code, # fips.code
)
fixest::etable(reg_wage_pw, digits = 4, digits.stats = 4)

reg_wage_pw <- fixest::feols(
  wage.perworker ~ e.treated +
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~naics.code, # naics.code
)
fixest::etable(reg_wage_pw, digits = 4, digits.stats = 4)

reg_wage_pw <- fixest::feols(
  wage.perworker ~ e.treated +
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.zipcode, # facility.zipcode
)
fixest::etable(reg_wage_pw, digits = 4, digits.stats = 4)

reg_wage_pw <- fixest::feols(
  wage.perworker ~ e.treated +
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_wage_pw, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Wage per hour
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~fips.code, # fips.code
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.id, # facility.id
)
fixest::etable(reg_wagephr, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Labour cost: Industry Pay
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~fips.code, # fips.code
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~fips.code, # fips.code
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~fips.code, # fips.code
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~fips.code, # fips.code
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~fips.code, # fips.code
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~fips.code, # fips.code
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~fips.code, # fips.code
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
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
  l.emp ~ e.treated * dist.to.border +
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
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state.id,
)

fixest::etable(reg_emp, digits = 4, digits.stats = 4)