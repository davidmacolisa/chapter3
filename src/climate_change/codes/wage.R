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
setwd(dir = "C:/Users/Davidmac.Ekeocha/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
source(file = "did_design_onsite.R", echo = T)
#======================================================================================================================#
reg.treat <- fixest::feols(
  total.air.emissions.onsite.intensity ~
    # treated
    i(rel_year, ref = c(2013, Inf)) +
      # trade.secret +
      # sanitised +
      entire.facility +
      federal.facility +
      govt.owned.facility +
      gdp
      # population +
      # annual_avg_estabs
      # personal_income +
      # emp
      |
      year +
        fips_code +
        relaxed_cpcp_id +
        fips_code +
        facility.state,
  data = triQ.on,
  cluster = ~c(facility.id, facility.state),
)

fixest::etable(reg.treat)
fixest::iplot(reg.treat, xlim = c(2011, 2017), ylim = c(-0.3, 0.4))