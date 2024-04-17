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
#======================================================================================================================#
## Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading Data
#======================================================================================================================#
file <- "./Data_PhD/US/BLS/onsite/triQc.rds"
triQc <- read_rds(file = file)
#======================================================================================================================#
table(triQc$facility.state, triQc$ch.year)
n_distinct(triQc$chemical.name)
sort(unique(triQc$chemical.name))
sort(unique(triQc$state.id))
sort(unique(triQc$year))
sort(unique(triQc$rel.year))

reg.treat <- fixest::feols(
  total_annual_wages ~
    sw0(
      e.treated
      # treated * post
      # i(treated * year, ref = c(2013, Inf))
      # trade.secret +
      # sanitised +
      # entire.facility +
      # federal.facility +
      # govt.owned.facility +
      # gdp.1 +
      # pinc.1
      # population +
      # annual_avg_estabs
      # personal_income +
      # emp
    )
      |
      csw(,
        # year,
        facility.zipcode,
        #   facility.city +
        #   facility.county
        # fips_code,
        # state.id,
        treated.cluster.year.fe,
        # treated.cluster.id,
      )
  ,
  data = triQc,
  cluster = ~c(fips_code),
)

fixest::etable(reg.treat)
fixest::iplot(reg.treat, xlim = c(2011, 2017), ylim = c(-0.05, 0.1))

reg.treat <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    e.treated +
      # i(rel.year, ref = c(2013, Inf))
      # treated * post
      # i(treated * year, ref = c(2013, Inf))
      trade.secret +
      sanitised +
      entire.facility +
      federal.facility +
      govt.owned.facility +
      gdp +
      population +
      annual_avg_estabs
      # personal_income +
      # emp
      |
      year +
        facility.zipcode +
        facility.city +
        facility.county +
        fips_code +
        facility.state +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc,
  cluster = ~c(fips_code, facility.state),
)

fixest::etable(reg.treat)
fixest::iplot(reg.treat, xlim = c(2011, 2017), ylim = c(-0.05, 0.1))
reg.treat <- fixest::feols(
  l.total.air.emissions.onsite.intensity ~
    # e.treated +
    i(rel.year, ref = c(2013, Inf)) +
      # treated * post
      # i(treated * year, ref = c(2013, Inf))
      trade.secret +
      sanitised +
      entire.facility +
      federal.facility +
      govt.owned.facility +
      gdp +
      population +
      annual_avg_estabs
      # personal_income +
      # emp
      |
      year +
        facility.zipcode +
        facility.city +
        facility.county +
        fips_code +
        facility.state +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc,
  cluster = ~c(fips_code, facility.state),
)

fixest::etable(reg.treat)
fixest::iplot(reg.treat, xlim = c(2011, 2017), ylim = c(-0.05, 0.1))