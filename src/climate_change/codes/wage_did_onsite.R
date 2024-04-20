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
library(car)
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
### Labour Cost---Wage per hr, weekly wages, and  total wages
#======================================================================================================================#
table(triQc$facility.state, triQc$ch.year)
n_distinct(triQc$chemical.name)
sort(unique(triQc$chemical.name))
sort(unique(triQc$state.id))
sort(unique(triQc$year))
sort(unique(triQc$rel.year))
#======================================================================================================================#
### Labour cost: Industry production workers' wages
#======================================================================================================================#
sum_up(triQc, prodw)
reg.wage <- fixest::feols(
  prodw ~ e.treated +
    sw0(
      gdp.1 +
        pinc.1 +
        equip.1 +
        plant.1 +
        annual.avg.estabs.1 +
        private.naics
    )
    |
    csw(,
      year,
      facility.zipcode,
      fips.code,
      state.id,
      fips.state.fe,
      treated.cluster.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(year, fips.code, state.id),
)
fixest::etable(reg.wage)

reg.wage <- fixest::feols(
  l.prodw ~ e.treated +
    sw0(
      gdp.1 +
        pinc.1 +
        equip.1 +
        plant.1 +
        annual.avg.estabs.1 +
        private.naics
    )
    |
    csw(,
      year,
      facility.zipcode,
      fips.code,
      state.id,
      fips.state.fe,
      treated.cluster.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(year, fips.code, state.id),
)
fixest::etable(reg.wage)

reg.wage <- fixest::feols(
  l.prodw ~
    i(rel.year, ref = c(2013, Inf)) +
      gdp.1 +
      pinc.1 +
      equip.1 +
      plant.1 +
      annual.avg.estabs.1 +
      private.naics
      |
      year +
        facility.zipcode +
        fips.code +
        state.id +
        fips.state.fe +
        treated.cluster.id +
        treated.cluster.year.fe
  ,
  data = triQc,
  cluster = ~c(year, fips.code, state.id),
)

fixest::etable(reg.wage)
fixest::iplot(reg.wage, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Effect of MW on Labour Cost", xlab = "relative year") %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(reg.wage)[grep(pattern = "rel.year", names(coef(reg.wage)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(reg.wage, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Wage per hour
#======================================================================================================================#
reg.wagephr <- fixest::feols(
  wage.perhr ~ e.treated +
    sw0(
      gdp.1 +
        pinc.1 +
        equip.1 +
        plant.1 +
        annual.avg.estabs.1 +
        private.naics
    )
    |
    csw(,
      year,
      facility.zipcode,
      fips.code,
      state.id,
      fips.state.fe,
      treated.cluster.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(year, fips.code, state.id),
)

fixest::etable(reg.wagephr)

reg.wagephr <- fixest::feols(
  l.wage.perhr ~ e.treated +
    sw0(
      gdp.1 +
        pinc.1 +
        equip.1 +
        plant.1 +
        annual.avg.estabs.1 +
        private.naics
    )
    |
    csw(,
      year,
      facility.zipcode,
      fips.code,
      state.id,
      fips.state.fe,
      treated.cluster.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(year, fips.code, state.id),
)

fixest::etable(reg.wagephr)

reg.wagephr <- fixest::feols(
  l.wage.perhr ~
    i(rel.year, ref = c(2013, Inf)) +
      gdp.1 +
      pinc.1 +
      equip.1 +
      plant.1 +
      annual.avg.estabs.1 +
      private.naics
      |
      year +
        facility.zipcode +
        fips.code +
        state.id +
        fips.state.fe +
        treated.cluster.id +
        treated.cluster.year.fe
  ,
  data = triQc,
  cluster = ~c(year, fips.code, state.id),
)

fixest::etable(reg.wagephr)
fixest::iplot(reg.wagephr, xlim = c(2011, 2017), ylim = c(-0.1, 0.1), col = "blue",
              main = "Effect of MW on Labour Cost Per Hour", xlab = "relative year") %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(reg.wagephr)[grep(pattern = "rel.year", names(coef(reg.wagephr)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(reg.wagephr, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Labour cost: Industry Pay
#======================================================================================================================#
reg.pay <- fixest::feols(
  pay ~ e.treated +
    sw0(
      gdp.1 +
        pinc.1 +
        equip.1 +
        plant.1 +
        annual.avg.estabs.1 +
        private.naics
    )
    |
    csw(,
      year,
      facility.zipcode,
      fips.code,
      state.id,
      fips.state.fe,
      treated.cluster.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(year, fips.code, state.id),
)

fixest::etable(reg.pay)

reg.pay <- fixest::feols(
  l.pay ~ e.treated +
    sw0(
      gdp.1 +
        pinc.1 +
        equip.1 +
        plant.1 +
        annual.avg.estabs.1 +
        private.naics
    )
    |
    csw(,
      year,
      facility.zipcode,
      fips.code,
      state.id,
      fips.state.fe,
      treated.cluster.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(year, fips.code, state.id),
)

fixest::etable(reg.pay)

reg.pay <- fixest::feols(
  l.pay ~ i(rel.year, ref = c(2013, Inf)) +
    gdp.1 +
    pinc.1 +
    equip.1 +
    plant.1 +
    annual.avg.estabs.1 +
    private.naics
    |
    year +
      facility.zipcode +
      fips.code +
      state.id +
      fips.state.fe +
      treated.cluster.id +
      treated.cluster.year.fe
  ,
  data = triQc,
  cluster = ~c(year, fips.code, state.id),
)

fixest::etable(reg.pay)
fixest::iplot(reg.pay, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Effect of MW on Total Payroll", xlab = "relative year") %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(reg.pay)[grep(pattern = "rel.year", names(coef(reg.pay)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(reg.pay, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Industry: Employment
#======================================================================================================================#
reg.emp <- fixest::feols(
  l.emp ~ e.treated +
    sw0(
      gdp.1 +
        pinc.1 +
        equip.1 +
        plant.1 +
        annual.avg.estabs.1 +
        private.naics
    )
    |
    csw(,
      year,
      facility.zipcode,
      fips.code,
      state.id,
      fips.state.fe,
      treated.cluster.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(year, fips.code, state.id),
)

fixest::etable(reg.emp)

reg.emp <- fixest::feols(
  l.emp ~ i(rel.year, ref = c(2013, Inf)) +
    gdp.1 +
    pinc.1 +
    equip.1 +
    plant.1 +
    annual.avg.estabs.1 +
    private.naics
    |
    year +
      facility.zipcode +
      fips.code +
      state.id +
      fips.state.fe +
      treated.cluster.id +
      treated.cluster.year.fe
  ,
  data = triQc,
  cluster = ~c(year, fips.code, state.id),
)

fixest::etable(reg.emp)
fixest::iplot(reg.emp, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Effect of MW on Industry Employment", xlab = "relative year") %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(reg.emp)[grep(pattern = "rel.year", names(coef(reg.emp)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(reg.emp, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#
### Material cost: Industry material cost
#======================================================================================================================#
reg.matcost <- fixest::feols(
  l.matcost ~ e.treated +
    sw0(
      gdp.1 +
        pinc.1 +
        equip.1 +
        plant.1 +
        annual.avg.estabs.1 +
        private.naics
    )
    |
    csw(,
      year,
      facility.zipcode,
      fips.code,
      state.id,
      fips.state.fe,
      treated.cluster.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~c(year, fips.code, state.id),
)

fixest::etable(reg.matcost)

reg.matcost <- fixest::feols(
  l.matcost ~ i(rel.year, ref = c(2013, Inf)) +
    gdp.1 +
    pinc.1 +
    equip.1 +
    plant.1 +
    annual.avg.estabs.1 +
    private.naics
    |
    year +
      facility.zipcode +
      fips.code +
      state.id +
      fips.state.fe +
      treated.cluster.id +
      treated.cluster.year.fe
  ,
  data = triQc,
  cluster = ~c(year, fips.code, state.id),
)

fixest::etable(reg.matcost)
fixest::iplot(reg.matcost, xlim = c(2011, 2017), ylim = c(-0.5, 0.5), col = "blue",
              main = "Effect of MW on Material Cost", xlab = "relative year") %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre.treat.coef <- coef(reg.matcost)[grep(pattern = "rel.year", names(coef(reg.matcost)))]
pre.treat.coef <- pre.treat.coef[5:6]
linearHypothesis(reg.matcost, paste0(names(pre.treat.coef), " = 0"), test = "F")
#======================================================================================================================#