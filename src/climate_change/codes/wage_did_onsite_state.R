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
library(TwoWayFEWeights)
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
      facility.state.fe,
      border.state.fe,
      border.state.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state,
)
fixest::etable(treat_sel, digits = 4, digits.stats = 4)
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
      facility.state.fe,
      border.state.fe,
      border.state.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state,
)
fixest::etable(reg_wagephr, digits = 4, digits.stats = 4)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.wage.perhr",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_wagephr <- fixest::feols(
  l.wage.perhr ~
    i(rel.year, ref = c(-1, Inf)) +
      gdppc.1 +
      annual.avg.estabs.1 +
      population.1 +
      cpi.1 +
      entire.facility +
      private.facility +
      federal.facility
      |
      year +
        facility.state.fe +
        border.state.fe +
        border.state.year.fe
  ,
  data = triQc,
  cluster = ~facility.state,
)
fixest::etable(reg_wagephr, digits = 4, digits.stats = 4)
wage_perhr <- fixest::iplot(reg_wagephr, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
                            main = "Hourly wage", xlab = "relative year",
                            lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_wagephr)[grep(pattern = "rel.year", names(coef(reg_wagephr)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_wagephr, paste0(names(pre_treat_coef), " = 0"), test = "F")
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
      facility.state.fe,
      border.state.fe,
      border.state.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state,
)
fixest::etable(reg_pay, digits = 4, digits.stats = 4)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "pay",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_pay <- fixest::feols(
  l.pay ~ i(rel.year, ref = c(-1, Inf)) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      facility.state.fe +
      border.state.fe +
      border.state.year.fe
  ,
  data = triQc,
  cluster = ~facility.state,
)

fixest::etable(reg_pay, digits = 4, digits.stats = 4)
fixest::iplot(reg_pay, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Total Payroll", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_pay)[grep(pattern = "rel.year", names(coef(reg_pay)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_pay, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
### Material cost: Industry material cost (log)
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
      facility.state.fe,
      border.state.fe,
      border.state.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state,
)
fixest::etable(reg_matcost, digits = 4, digits.stats = 4)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.matcost",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_matcost <- fixest::feols(
  l.matcost ~ i(rel.year, ref = c(-1, Inf)) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      facility.state.fe +
      border.state.fe +
      border.state.year.fe
  ,
  data = triQc,
  cluster = ~facility.state,
)
fixest::etable(reg_matcost, digits = 4, digits.stats = 4)
fixest::iplot(reg_matcost, xlim = c(-3, 3), ylim = c(-0.6, 0.6), col = "blue",
              main = "Material Cost (log)", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_matcost)[grep(pattern = "rel.year", names(coef(reg_matcost)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_matcost, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_state_did_industry_costs.pdf", width = 14, height = 4)
par(mfrow = c(1, 3))
fixest::iplot(reg_wagephr, xlim = c(-3, 3), ylim = c(-0.15, 0.15), col = "blue",
              main = "Hourly Wage (log)", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(reg_pay, xlim = c(-3, 3), ylim = c(-0.4, 0.4), col = "blue",
              main = "Total Payroll (log)", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(reg_matcost, xlim = c(-3, 3), ylim = c(-0.6, 0.6), col = "blue",
              main = "Material Cost (log)", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
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
      facility.state.fe,
      border.state.fe,
      border.state.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state,
)

fixest::etable(reg_emp, digits = 4, digits.stats = 4)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.emp",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_emp <- fixest::feols(
  l.emp ~ i(rel.year, ref = c(-1, Inf)) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      facility.state.fe +
      border.state.fe +
      border.state.year.fe
  ,
  data = triQc,
  cluster = ~facility.state,
)

fixest::etable(reg_emp, digits = 4, digits.stats = 4)
fixest::iplot(reg_emp, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Industry Employment (log)", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
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
        population.1 +
        cpi.1 +
        entire.facility +
        private.facility +
        federal.facility
    )
    |
    csw(,
      year,
      facility.state.fe,
      border.state.fe,
      border.state.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state,
)
fixest::etable(reg_phours, digits = 4, digits.stats = 4)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.prodh",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_phours <- fixest::feols(
  l.prodh ~ i(rel.year, ref = c(-1, Inf)) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      facility.state.fe +
      border.state.fe +
      border.state.year.fe
  ,
  data = triQc,
  cluster = ~facility.state,
)
fixest::etable(reg_phours, digits = 4, digits.stats = 4)
fixest::iplot(reg_phours, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
              main = "Workers' Hours (log)", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_phours)[grep(pattern = "rel.year", names(coef(reg_phours)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_phours, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_state_did_emp_hours.pdf", width = 10, height = 4.5)
par(mfrow = c(1, 2))
fixest::iplot(reg_emp, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Industry Employment", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(reg_phours, xlim = c(-3, 3), ylim = c(-0.4, 0.3), col = "blue",
              main = "Workers' Hours", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
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
      facility.state.fe,
      border.state.fe,
      border.state.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state,
)
fixest::etable(reg_output, digits = 4, digits.stats = 4)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.vadd",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_output <- fixest::feols(
  log(vadd) ~ i(rel.year, ref = c(-1, Inf)) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      facility.state.fe +
      border.state.fe +
      border.state.year.fe
  ,
  data = triQc,
  cluster = ~facility.state,
)
fixest::etable(reg_output, digits = 4, digits.stats = 4)
fixest::iplot(reg_output, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Industry Output (log)", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_output)[grep(pattern = "rel.year", names(coef(reg_output)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_output, paste0(names(pre_treat_coef), " = 0"), test = "F")
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
      facility.state.fe,
      border.state.fe,
      border.state.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state,
)
fixest::etable(reg_outputprhr, digits = 4, digits.stats = 4)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.output.perhr",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_outputprhr <- fixest::feols(
  l.output.perhr ~ i(rel.year, ref = c(-1, Inf)) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      facility.state.fe +
      border.state.fe +
      border.state.year.fe
  ,
  data = triQc,
  cluster = ~facility.state,
)
fixest::etable(reg_outputprhr, digits = 4, digits.stats = 4)
fixest::iplot(reg_outputprhr, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Output per Hour (log)", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_outputprhr)[grep(pattern = "rel.year", names(coef(reg_outputprhr)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_outputprhr, paste0(names(pre_treat_coef), " = 0"), test = "F")
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
      facility.state.fe,
      border.state.fe,
      border.state.year.fe
    )
  ,
  data = triQc,
  cluster = ~facility.state,
)

fixest::etable(reg_outputperworker, digits = 4, digits.stats = 4)
#----------------------------------------------------------------------------------------------------------------------#
# Get de Chaisemartin and D'Haultfoeuille Decomposition
dCDH_decomp <- twowayfeweights(
  Y = "l.output.perworker",
  G = "facility.state.fe",
  T = "year",
  D = "e.treated",
  type = "feTR",
  data = triQc,
)
dCDH_decomp
# Weakly Positive weights
sum(dCDH_decomp$weight[dCDH_decomp$weight >= 0])

# Negative weights
sum(dCDH_decomp$weight[dCDH_decomp$weight < 0])
#----------------------------------------------------------------------------------------------------------------------#
reg_outputperworker <- fixest::feols(
  l.output.perworker ~ i(rel.year, ref = c(-1, Inf)) +
    gdppc.1 +
    annual.avg.estabs.1 +
    population.1 +
    cpi.1 +
    entire.facility +
    private.facility +
    federal.facility
    |
    year +
      facility.state.fe +
      border.state.fe +
      border.state.year.fe
  ,
  data = triQc,
  cluster = ~facility.state,
)
fixest::etable(reg_outputperworker, digits = 4, digits.stats = 4)
fixest::iplot(reg_outputperworker, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Output per Worker (log)", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
### Testing for pre-trends
pre_treat_coef <- coef(reg_outputperworker)[grep(pattern = "rel.year", names(coef(reg_outputperworker)))]
pre_treat_coef <- pre_treat_coef[5:6]
linearHypothesis(reg_outputperworker, paste0(names(pre_treat_coef), " = 0"), test = "F")
#======================================================================================================================#
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_state_did_output.pdf", width = 14, height = 4)
par(mfrow = c(1, 3))
fixest::iplot(reg_output, xlim = c(-3, 3), ylim = c(-0.3, 0.3), col = "blue",
              main = "Industry Output (log)", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(reg_outputprhr, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Output per Hour (log)", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
fixest::iplot(reg_outputperworker, xlim = c(-3, 3), ylim = c(-0.5, 0.5), col = "blue",
              main = "Output per Worker (log)", xlab = "relative year",
              lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)
dev.off()
#======================================================================================================================#