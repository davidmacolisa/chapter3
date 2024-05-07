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
file <- "./Data_PhD/US/BLS/onsite/triQc_on.rds"
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
### Treatment Selection
#======================================================================================================================#
treat_sel <- fixest::feols(
  e.treated ~ sw0(
    gdp.1 +
      gdppc.1 +
      pinc.1 +
      equip.1 +
      plant.1 +
      population.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      entire.facility +
      private.facility +
      federal.facility
  )
    |
    csw(
      year,
      treated.cluster.id,
      facility.state.id,
      treated.cluster.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state.id,
)
fixest::etable(treat_sel, digits = 4, digits.stats = 4)
#======================================================================================================================#
### Labour cost: Industry production workers' wages
#======================================================================================================================#
sum_up(triQf, prodw)
reg_wage <- fixest::feols(
  prodw ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
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
fixest::etable(reg_wage, digits = 4, digits.stats = 4)

reg_wage <- fixest::feols(
  l.prodw ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
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
fixest::etable(reg_wage, digits = 4, digits.stats = 4)

reg_wage <- fixest::feols(
  l.prodw ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      entire.facility +
      private.facility +
      federal.facility
      |
      year +
        treated.cluster.id +
        facility.state.id +
        treated.cluster.year.fe
  ,
  data = triQc,
  cluster = ~facility.state.id,
)

fixest::etable(reg_wage, digits = 4, digits.stats = 4)
fixest::iplot(reg_wage, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Labour Cost", xlab = "relative year") %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_wage)[grep(pattern = "rel.year", names(coef(reg_wage)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_wage, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Labour cost: Industry production wages per worker
#======================================================================================================================#
reg_wage_pw <- fixest::feols(
  wage.perworker ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
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
fixest::etable(reg_wage_pw, digits = 4, digits.stats = 4)

reg_wage_pw <- fixest::feols(
  l.wage.perworker ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
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
fixest::etable(reg_wage_pw, digits = 4, digits.stats = 4)

reg_wage_pw <- fixest::feols(
  l.wage.perworker ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      entire.facility +
      private.facility +
      federal.facility
      |
      year +
        treated.cluster.id +
        facility.state.id +
        treated.cluster.year.fe
  ,
  data = triQc,
  cluster = ~facility.state.id,
)

fixest::etable(reg_wage_pw, digits = 4, digits.stats = 4)
fixest::iplot(reg_wage_pw, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Labour Cost Per Worker", xlab = "relative year") %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_wage_pw)[grep(pattern = "rel.year", names(coef(reg_wage_pw)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_wage_pw, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Wage per hour
#======================================================================================================================#
reg_wagephr <- fixest::feols(
  wage.perhr ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
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
fixest::etable(reg_wagephr, digits = 4, digits.stats = 4)

reg_wagephr <- fixest::feols(
  l.wage.perhr ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
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

fixest::etable(reg_wagephr, digits = 4, digits.stats = 4)

reg_wagephr <- fixest::feols(
  l.wage.perhr ~
    i(rel.year, ref = c(2013, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      cpi.1 +
      entire.facility +
      private.facility +
      federal.facility
      |
      year +
        treated.cluster.id +
        facility.state.id +
        treated.cluster.year.fe
  ,
  data = triQc,
  cluster = ~facility.state.id,
)

fixest::etable(reg_wagephr, digits = 4, digits.stats = 4)
fixest::iplot(reg_wagephr, xlim = c(2011, 2017), ylim = c(-0.2, 0.2), col = "blue",
              main = "Labour Cost Per Hour", xlab = "relative year") %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_wagephr)[grep(pattern = "rel.year", names(coef(reg_wagephr)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_wagephr, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Labour cost: Industry Pay
#======================================================================================================================#
reg_pay <- fixest::feols(
  pay ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
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

fixest::etable(reg_pay, digits = 4, digits.stats = 4)

reg_pay <- fixest::feols(
  l.pay ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
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

fixest::etable(reg_pay, digits = 4, digits.stats = 4)

reg_pay <- fixest::feols(
  l.pay ~ i(rel.year, ref = c(2013, Inf)) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      treated.cluster.id +
      facility.state.id +
      treated.cluster.year.fe
  ,
  data = triQc,
  cluster = ~facility.state.id,
)

fixest::etable(reg_pay, digits = 4, digits.stats = 4)
fixest::iplot(reg_pay, xlim = c(2011, 2017), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Payroll", xlab = "relative year") %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_pay)[grep(pattern = "rel.year", names(coef(reg_pay)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_pay, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Material cost: Industry material cost
#======================================================================================================================#
reg_matcost <- fixest::feols(
  l.matcost ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
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

fixest::etable(reg_matcost, digits = 4, digits.stats = 4)

reg_matcost <- fixest::feols(
  l.matcost ~ i(rel.year, ref = c(2013, Inf)) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      treated.cluster.id +
      facility.state.id +
      treated.cluster.year.fe
  ,
  data = triQc,
  cluster = ~facility.state.id,
)

fixest::etable(reg_matcost, digits = 4, digits.stats = 4)
fixest::iplot(reg_matcost, xlim = c(2011, 2017), ylim = c(-0.6, 0.6), col = "blue",
              main = "Material Cost", xlab = "relative year") %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_matcost)[grep(pattern = "rel.year", names(coef(reg_matcost)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_matcost, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Industry: Employment
#======================================================================================================================#
reg_emp <- fixest::feols(
  l.emp ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
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

reg_emp <- fixest::feols(
  l.emp ~ i(rel.year, ref = c(2013, Inf)) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      treated.cluster.id +
      facility.state.id +
      treated.cluster.year.fe
  ,
  data = triQc,
  cluster = ~facility.state.id,
)

fixest::etable(reg_emp, digits = 4, digits.stats = 4)
fixest::iplot(reg_emp, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Industry Employment", xlab = "relative year") %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_emp)[grep(pattern = "rel.year", names(coef(reg_emp)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_emp, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Industry: Production hours
#======================================================================================================================#
reg_phours <- fixest::feols(
  l.prodh ~ e.treated +
    sw0(
      gdppc.1 +
        annual.avg.estabs.1 +
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

fixest::etable(reg_phours, digits = 4, digits.stats = 4)

reg_phours <- fixest::feols(
  l.prodh ~ i(rel.year, ref = c(2013, Inf)) +
    gdppc.1 +
    annual.avg.estabs.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      treated.cluster.id +
      facility.state.id +
      treated.cluster.year.fe
  ,
  data = triQc,
  cluster = ~facility.state.id,
)
fixest::etable(reg_phours, digits = 4, digits.stats = 4)
fixest::iplot(reg_phours, xlim = c(2011, 2017), ylim = c(-0.3, 0.3), col = "blue",
              main = "Workers' Hours", xlab = "relative year") %>%
  abline(v = 2013, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_phours)[grep(pattern = "rel.year", names(coef(reg_phours)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_phours, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#