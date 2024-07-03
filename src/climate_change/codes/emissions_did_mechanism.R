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
### Financial Constraints
### I proxy financial constraints by the ratio of industries profit margin. That is, the ratio of industry revenue to
# profit, which measures the proportion of industry revenue allocated to profit. Industries with profit margin lower
# than the median profit margin are more financially constrained, and those above are less.
#======================================================================================================================#
triQc <- triQc %>%
  group_by(year, naics.code) %>%
  rename(revenue = vship) %>%
  mutate(
	total.cost = matcost + prodw + energy,
	profit = revenue - total.cost,
	revenue.to.profit = (revenue / profit) * 100,
	# Operational efficiency ratio
	payroll.to.revenue = pay / revenue,
	wages.to.revenue = prodw / revenue,
  ) %>%
  ungroup() %>%
  mutate(
	high.profit.margin = case_when(revenue.to.profit > mean(revenue.to.profit) ~ 1, TRUE ~ 0),
	high.payroll.revenue = case_when(payroll.to.revenue > mean(payroll.to.revenue) ~ 1, TRUE ~ 0),
	high.wages.revenue = case_when(wages.to.revenue > mean(wages.to.revenue) ~ 1, TRUE ~ 0),
  )

triQc %>% sum_up(
  c(high.payroll.revenue, high.wages.revenue, high.profit.margin)
)
#======================================================================================================================#
### Waste Management Activities - Onsite
#======================================================================================================================#
### Total waste management onsite---high profit margin
#======================================================================================================================#
sdid_waste_mgt_onsite_hrpr <- fixest::feols(
  l.total.waste.management.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_waste_mgt_onsite_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_waste_mgt_onsite_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment onsite---high profit margin
#======================================================================================================================#
sdid_treatment_onsite_hrpr <- fixest::feols(
  l.treatment.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_treatment_onsite_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_treatment_onsite_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Air emissions treatment onsite---high profit margin
#======================================================================================================================#
sdid_air_emissions_treatment_hrpr <- fixest::feols(
  l.air.emissions.treatment.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_air_emissions_treatment_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_emissions_treatment_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Biological treatment onsite---high profit margin
#======================================================================================================================#
sdid_bio_treatment_hrpr <- fixest::feols(
  l.biological.treatment.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_bio_treatment_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_bio_treatment_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Chemical treatment onsite---high profit margin
#======================================================================================================================#
sdid_chem_treatment_hrpr <- fixest::feols(
  l.chemical.treatment.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_chem_treatment_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_chem_treatment_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Physical treatment onsite---high profit margin
#======================================================================================================================#
sdid_physical_treatment_hrpr <- fixest::feols(
  l.physical.treatment.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_physical_treatment_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_physical_treatment_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Incineration thermal treatment onsite---high profit margin
#======================================================================================================================#
sdid_incineration_treatment_hrpr <- fixest::feols(
  l.incineration.thermal.treatment.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_incineration_treatment_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_incineration_treatment_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery onsite---high profit margin
#======================================================================================================================#
sdid_energy_recovery_hrpr <- fixest::feols(
  l.energy.recovery.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_energy_recovery_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_energy_recovery_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery method: Industrial kiln onsite---high profit margin
#======================================================================================================================#
sdid_ind_klin_hrpr <- fixest::feols(
  l.industrial.kiln.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|

	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_ind_klin_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_ind_klin_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery method: Industrial boiler onsite---high profit margin
#======================================================================================================================#
sdid_ind_boiler_hrpr <- fixest::feols(
  l.industrial.boiler.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_ind_boiler_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_ind_boiler_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery method: Industrial furnace onsite---high profit margin
#======================================================================================================================#
sdid_ind_furnace_hrpr <- fixest::feols(
  l.industrial.furnace.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_ind_furnace_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_ind_furnace_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling onsite---high profit margin
#======================================================================================================================#
sdid_recycle_onsite_hrpr <- fixest::feols(
  l.recycling.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recycle_onsite_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_recycle_onsite_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling process: Reuse onsite---high profit margin
#======================================================================================================================#
sdid_reuse_onsite_hrpr <- fixest::feols(
  l.reuse.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_reuse_onsite_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_reuse_onsite_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling process: Metal recovery onsite---high profit margin
#======================================================================================================================#
sdid_metal_recovery_hrpr <- fixest::feols(
  l.metal.recovery.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_metal_recovery_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_metal_recovery_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling process: Solvent recovery onsite---high profit margin
#======================================================================================================================#
sdid_solvent_recovery_hrpr <- fixest::feols(
  l.solvent.recovery.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_solvent_recovery_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_solvent_recovery_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Onsite: Source Reduction Activities---high profit margin (revenue to profit ratio)
#TODO: Check the effect on chemical manufacturing aid, formulation component, article component and ancilliary use
#======================================================================================================================#
sdid_source_reduction_hrpr <- fixest::feols(
  source.reduction ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_source_reduction_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_source_reduction_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material Substitution---high profit margin (revenue to profit ratio)
#======================================================================================================================#
sdid_mat_submod_hrpr <- fixest::feols(
  material.subandmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_submod_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_submod_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Organic Solvent Substitution---high profit margin (revenue to profit ratio)
#======================================================================================================================#
sdid_organic_solvent_hrpr <- fixest::feols(
  sub.organic.solvent.matsubmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_organic_solvent_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_organic_solvent_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Feedstock and Reactive Chemical Substitution---high profit margin (revenue to profit ratio)
#======================================================================================================================#
sdid_feedstock_reactchems_hrpr <- fixest::feols(
  sub.rawm.feedstock.reactchem.matsubmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_feedstock_reactchems_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_feedstock_reactchems_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: manufacturing, processing aids, and ancillary chemical Substitution---high profit margin
#======================================================================================================================#
sdid_manu_aid_hrpr <- fixest::feols(
  sub.manu.proccess.ancilliary.chems.matsubmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_manu_aid_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_manu_aid_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Increase Purity of Chemicals---high profit margin
#======================================================================================================================#
sdid_mat_purity_hrpr <- fixest::feols(
  mod.content.grade.purity.chems.matsubmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_purity_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_purity_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Clean Fuel---high profit margin
#======================================================================================================================#
sdid_clean_fuel_hrpr <- fixest::feols(
  sub.fuel.matsubmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_clean_fuel_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_clean_fuel_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Energy Cost Intensity---high profit margin
#======================================================================================================================#
sdid_energy_cost_intensity_hrpr <- fixest::feols(
  l.energy.cost.intensity ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_energy_cost_intensity_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_energy_cost_intensity_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Other material modification---high profit margin
#======================================================================================================================#
sdid_mat_others_hrpr <- fixest::feols(
  other.matmods.matsubmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_others_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_others_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification---high profit margin
#======================================================================================================================#
sdid_prod_mod_hrpr <- fixest::feols(
  product.modification ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  , data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_prod_mod_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_prod_mod_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: New Product Line---high profit margin
#======================================================================================================================#
sdid_new_prod_hine_hrpr <- fixest::feols(
  devd.newproductline.pmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_new_prod_hine_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_new_prod_hine_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: Modified Packaging---high profit margin
#======================================================================================================================#
sdid_mod_packaging_hrpr <- fixest::feols(
  mod.packaging.pmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mod_packaging_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mod_packaging_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: Other Product Modification---high profit margin
#======================================================================================================================#
sdid_prod_mod_others_hrpr <- fixest::feols(
  other.pmods.pmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_prod_mod_others_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_prod_mod_others_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Process and Equipment Modification---high profit margin
#======================================================================================================================#
sdid_process_equip_mod_hrpr <- fixest::feols(
  process.equip.modification ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_process_equip_mod_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_process_equip_mod_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Optimised Process Efficiency---high profit margin
#======================================================================================================================#
sdid_optimised_process_efficiency_hrpr <- fixest::feols(
  optimised.process.efficiency.pequipmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_optimised_process_efficiency_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_optimised_process_efficiency_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Recirculation in Process---high profit margin
#======================================================================================================================#
sdid_recirculate_inprocess_hrpr <- fixest::feols(
  recirculationinprocess.pequipmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recirculate_inprocess_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_recirculate_inprocess_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: New Technology and Technique in manufacturing process---high profit margin
#======================================================================================================================#
sdid_new_tech_hrpr <- fixest::feols(
  newtech.technique.process.pequipmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_new_tech_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_new_tech_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Recycling to reuse---high profit margin
#======================================================================================================================#
sdid_recylce_dummy_hrpr <- fixest::feols(
  recycling.dummy ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recylce_dummy_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_recylce_dummy_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Research & Development---high profit margin
#======================================================================================================================#
sdid_research_hrpr <- fixest::feols(
  r.and.d ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_research_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_research_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Total factor productivity---high profit margin
#======================================================================================================================#
sdid_tfp_hrpr <- fixest::feols(
  l.tfp5 ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_tfp_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_tfp_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Process Modification, Others---high profit margin
#======================================================================================================================#
sdid_process_mod_others_hrpr <- fixest::feols(
  other.pequipmods.pequipmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_process_mod_others_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_process_mod_others_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Inventory Material Management---high profit margin
#======================================================================================================================#
sdid_inventory_mat_mgt_hrpr <- fixest::feols(
  inventory.material.mgt ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_inventory_mat_mgt_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_inventory_mat_mgt_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Better Labelling and Testing---high profit margin
#======================================================================================================================#
sdid_better_habelling_testing_hrpr <- fixest::feols(
  better.labelling.testing.immgt ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_better_habelling_testing_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_better_habelling_testing_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Containers size change---high profit marrgin
#======================================================================================================================#
sdid_containers_sizechange_hrpr <- fixest::feols(
  containers.sizechange.immgt ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_containers_sizechange_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_containers_sizechange_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Improved material handling operations---high profit margin
#======================================================================================================================#
sdid_mat_handling_hrpr <- fixest::feols(
  improved.materialhandling.operations.immgt ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_handling_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_handling_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Improved monitoring---high profit margin
#======================================================================================================================#
sdid_improved_monitoring_hrpr <- fixest::feols(
  improved.monitoring.immgt ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_improved_monitoring_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_improved_monitoring_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Inventory Management, Others---high profit margin
#======================================================================================================================#
sdid_inventory_mgt_others_hrpr <- fixest::feols(
  other.immgts.immgt ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_inventory_mgt_others_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_inventory_mgt_others_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Operating Practices & Training---high profit margin
#======================================================================================================================#
sdid_operating_practices_training_hrpr <- fixest::feols(
  operating.practices.training ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_operating_practices_training_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_operating_practices_training_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Improved Scheduling Operation---high profit margin
#======================================================================================================================#
sdid_improved_schedule_operation_hrpr <- fixest::feols(
  improved.schdule.operation.procedures.opt ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_improved_schedule_operation_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_improved_schedule_operation_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Changed Production Schedule---high profit margin
#======================================================================================================================#
sdid_changed_prod_schedule_hrpr <- fixest::feols(
  changed.production.schedule.opt ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_changed_prod_schedule_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_changed_prod_schedule_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Inline Product Quality and Process Analysis---high profit margin
#======================================================================================================================#
sdid_inline_prod_quality_analysis_hrpr <- fixest::feols(
  intro.inline.productquality.process.analysis.opt ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_inline_prod_quality_analysis_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_inline_prod_quality_analysis_hrpr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Type of Production Technology
# Labour intensive industries---High payroll or wages to revenue ratio
# Capacity intensive industries---Low payroll or wages to revenue ratio
#======================================================================================================================#
### Total waste management onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_waste_mgt_onsite_hprr <- fixest::feols(
  l.total.waste.management.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_waste_mgt_onsite_hprr, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_waste_mgt_onsite_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_treatment_onsite_hprr <- fixest::feols(
  l.treatment.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_treatment_onsite_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_treatment_onsite_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment method: Air emissions treatment onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_air_emissions_treatment_hprr <- fixest::feols(
  l.air.emissions.treatment.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_air_emissions_treatment_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_emissions_treatment_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment method: Biological treatment onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_bio_treatment_hprr <- fixest::feols(
  l.biological.treatment.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_bio_treatment_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_bio_treatment_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment method: Chemical treatment onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_chem_treatment_hprr <- fixest::feols(
  l.chemical.treatment.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_chem_treatment_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_chem_treatment_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Physical treatment onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_physical_treatment_hprr <- fixest::feols(
  l.physical.treatment.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_physical_treatment_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_physical_treatment_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Incineration thermal treatment onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_incineration_treatment_hprr <- fixest::feols(
  l.incineration.thermal.treatment.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_incineration_treatment_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_incineration_treatment_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_energy_recovery_hprr <- fixest::feols(
  l.energy.recovery.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_energy_recovery_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_energy_recovery_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery method: Industrial kiln onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_ind_klin_hprr <- fixest::feols(
  l.industrial.kiln.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|

	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_ind_klin_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_ind_klin_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery method: Industrial boiler onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_ind_boiler_hprr <- fixest::feols(
  l.industrial.boiler.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_ind_boiler_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_ind_boiler_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery method: Industrial furnace onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_ind_furnace_hprr <- fixest::feols(
  l.industrial.furnace.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_ind_furnace_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_ind_furnace_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_recycle_onsite_hprr <- fixest::feols(
  l.recycling.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recycle_onsite_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_recycle_onsite_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling process: Reuse onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_reuse_onsite_hprr <- fixest::feols(
  l.reuse.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_reuse_onsite_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_reuse_onsite_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling process: Metal recovery onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_metal_recovery_hprr <- fixest::feols(
  l.metal.recovery.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_metal_recovery_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_metal_recovery_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling process: Solvent recovery onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_solvent_recovery_hprr <- fixest::feols(
  l.solvent.recovery.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_solvent_recovery_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_solvent_recovery_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Onsite: Source Reduction Activities---high payroll to revenue ratio (labour intensive)
#======================================================================================================================#
sdid_source_reduction_hprr <- fixest::feols(
  source.reduction ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_source_reduction_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_source_reduction_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material Substitution---high payroll to revenue (payroll to revenue ratio)
#======================================================================================================================#
sdid_mat_submod_hprr <- fixest::feols(
  material.subandmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_submod_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_submod_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Organic Solvent Substitution---high payroll ratio (payroll to revenue ratio)
#======================================================================================================================#
sdid_organic_solvent_hprr <- fixest::feols(
  sub.organic.solvent.matsubmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_organic_solvent_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_organic_solvent_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Feedstock and Reactive Chemical Substitution---high payroll ratio (payroll to revenue ratio)
#======================================================================================================================#
sdid_feedstock_reactchems_hprr <- fixest::feols(
  sub.rawm.feedstock.reactchem.matsubmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_feedstock_reactchems_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_feedstock_reactchems_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: manufacturing, processing aids, and ancillary chemical Substitution---high payroll ratio
#======================================================================================================================#
sdid_manu_aid_hprr <- fixest::feols(
  sub.manu.proccess.ancilliary.chems.matsubmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_manu_aid_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_manu_aid_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Increase Purity of Chemicals---high payroll to revenue ratio
#======================================================================================================================#
sdid_mat_purity_hprr <- fixest::feols(
  mod.content.grade.purity.chems.matsubmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_purity_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_purity_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Clean Fuel---high payroll to revenue ratio
#======================================================================================================================#
sdid_clean_fuel_hprr <- fixest::feols(
  sub.fuel.matsubmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_clean_fuel_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_clean_fuel_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: Energy Cost Intensity---high payroll to revenue ratio
#======================================================================================================================#
sdid_energy_intensity_hprr <- fixest::feols(
  l.energy.cost.intensity ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_energy_intensity_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_energy_intensity_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Other material modification---high payroll to revenue ratio
#======================================================================================================================#
sdid_mat_others_hprr <- fixest::feols(
  other.matmods.matsubmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_others_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_others_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification---high payroll to revenue ratio
#======================================================================================================================#
sdid_prod_mod_hprr <- fixest::feols(
  product.modification ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  , data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_prod_mod_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_prod_mod_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: New Product Line---high payroll to revenue ratio
#======================================================================================================================#
sdid_new_prod_hine_hprr <- fixest::feols(
  devd.newproductline.pmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_new_prod_hine_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_new_prod_hine_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: Modified Packaging---high payroll to revenue ratio
#======================================================================================================================#
sdid_mod_packaging_hprr <- fixest::feols(
  mod.packaging.pmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mod_packaging_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mod_packaging_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: Other Product Modification---high payroll to revenue ratio
#======================================================================================================================#
sdid_prod_mod_others_hprr <- fixest::feols(
  other.pmods.pmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_prod_mod_others_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_prod_mod_others_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Process and Equipment Modification---high payroll to revenue ratio
#======================================================================================================================#
sdid_process_equip_mod_hprr <- fixest::feols(
  process.equip.modification ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_process_equip_mod_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_process_equip_mod_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Optimised Process Efficiency---high payroll to revenue ratio
#======================================================================================================================#
sdid_optimised_process_efficiency_hprr <- fixest::feols(
  optimised.process.efficiency.pequipmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_optimised_process_efficiency_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_optimised_process_efficiency_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Recirculation in Process---high payroll to revenue ratio
#======================================================================================================================#
sdid_recirculate_inprocess_hprr <- fixest::feols(
  recirculationinprocess.pequipmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recirculate_inprocess_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_recirculate_inprocess_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: New Technology and Technique in manufacturing process---high payroll to revenue ratio
#======================================================================================================================#
sdid_new_tech_hprr <- fixest::feols(
  newtech.technique.process.pequipmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_new_tech_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_new_tech_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Recycling to reuse---high payroll to revenue ratio
#======================================================================================================================#
sdid_recylce_dummy_hprr <- fixest::feols(
  recycling.dummy ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recylce_dummy_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_recylce_dummy_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Research & Development---high payroll to revenue ratio
#======================================================================================================================#
sdid_research_hprr <- fixest::feols(
  r.and.d ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_research_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_research_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Process Modification, Others---high payroll to revenue ratio
#======================================================================================================================#
sdid_process_mod_others_hprr <- fixest::feols(
  other.pequipmods.pequipmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_process_mod_others_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_process_mod_others_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Total factor productivity---high payroll to revenue ratio
#======================================================================================================================#
sdid_tfp_hprr <- fixest::feols(
  l.tfp5 ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_tfp_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_tfp_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Inventory Material Management---high payroll to revenue ratio
#======================================================================================================================#
sdid_inventory_mat_mgt_hprr <- fixest::feols(
  inventory.material.mgt ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_inventory_mat_mgt_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_inventory_mat_mgt_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Better Labelling and Testing---high payroll to revenue ratio
#======================================================================================================================#
sdid_better_habelling_testing_hprr <- fixest::feols(
  better.labelling.testing.immgt ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_better_habelling_testing_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_better_habelling_testing_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Containers size change---high payroll to revenue ratio
#======================================================================================================================#
sdid_containers_sizechange_hprr <- fixest::feols(
  containers.sizechange.immgt ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_containers_sizechange_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_containers_sizechange_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Improved material handling operations---high payroll to revenue ratio
#======================================================================================================================#
sdid_mat_handling_hprr <- fixest::feols(
  improved.materialhandling.operations.immgt ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_handling_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_handling_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Improved monitoring---high payroll to revenue ratio
#======================================================================================================================#
sdid_improved_monitoring_hprr <- fixest::feols(
  improved.monitoring.immgt ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_improved_monitoring_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_improved_monitoring_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Inventory Management, Others---high payroll to revenue ratio
#======================================================================================================================#
sdid_inventory_mgt_others_hprr <- fixest::feols(
  other.immgts.immgt ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_inventory_mgt_others_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_inventory_mgt_others_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Operating Practices & Training---high payroll to revenue ratio
#======================================================================================================================#
sdid_operating_practices_training_hprr <- fixest::feols(
  operating.practices.training ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_operating_practices_training_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_operating_practices_training_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Improved Scheduling Operation---high payroll to revenue ratio
#======================================================================================================================#
sdid_improved_schedule_operation_hprr <- fixest::feols(
  improved.schdule.operation.procedures.opt ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_improved_schedule_operation_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_improved_schedule_operation_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Changed Production Schedule---high payroll to revenue ratio
#======================================================================================================================#
sdid_changed_prod_schedule_hprr <- fixest::feols(
  changed.production.schedule.opt ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_changed_prod_schedule_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_changed_prod_schedule_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Inline Product Quality and Process Analysis---high payroll to revenue ratio
#======================================================================================================================#
sdid_inline_prod_quality_analysis_hprr <- fixest::feols(
  intro.inline.productquality.process.analysis.opt ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_inline_prod_quality_analysis_hprr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_inline_prod_quality_analysis_hprr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Type of Production Technology
# Labour intensive industries---High payroll or wages to revenue ratio
# Capacity intensive industries---Low payroll or wages to revenue ratio
#======================================================================================================================#
### Total waste management onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_waste_mgt_onsite_hwrr <- fixest::feols(
  l.total.waste.management.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_waste_mgt_onsite_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_waste_mgt_onsite_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_treatment_onsite_hwrr <- fixest::feols(
  l.treatment.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_treatment_onsite_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_treatment_onsite_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment method: Air emissions treatment onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_air_emissions_treatment_hwrr <- fixest::feols(
  l.air.emissions.treatment.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_air_emissions_treatment_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_emissions_treatment_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Biological treatment onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_bio_treatment_hwrr <- fixest::feols(
  l.biological.treatment.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_bio_treatment_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_bio_treatment_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Chemical treatment onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_chem_treatment_hwrr <- fixest::feols(
  l.chemical.treatment.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_chem_treatment_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_chem_treatment_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Physical treatment onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_physical_treatment_hwrr <- fixest::feols(
  l.physical.treatment.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_physical_treatment_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_physical_treatment_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Incineration thermal treatment onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_incineration_treatment_hwrr <- fixest::feols(
  l.incineration.thermal.treatment.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_incineration_treatment_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_incineration_treatment_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_energy_recovery_hwrr <- fixest::feols(
  l.energy.recovery.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_energy_recovery_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_energy_recovery_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery method: Industrial kiln onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_ind_klin_hwrr <- fixest::feols(
  l.industrial.kiln.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|

	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_ind_klin_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_ind_klin_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery method: Industrial boiler onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_ind_boiler_hwrr <- fixest::feols(
  l.industrial.boiler.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_ind_boiler_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_ind_boiler_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery method: Industrial furnace onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_ind_furnace_hwrr <- fixest::feols(
  l.industrial.furnace.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_ind_furnace_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_ind_furnace_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_recycle_onsite_hwrr <- fixest::feols(
  l.recycling.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recycle_onsite_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_recycle_onsite_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling process: Reuse onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_reuse_onsite_hwrr <- fixest::feols(
  l.reuse.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_reuse_onsite_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_reuse_onsite_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling process: Metal recovery onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_metal_recovery_hwrr <- fixest::feols(
  l.metal.recovery.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_metal_recovery_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_metal_recovery_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling process: Solvent recovery onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_solvent_recovery_hwrr <- fixest::feols(
  l.solvent.recovery.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_solvent_recovery_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_solvent_recovery_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Onsite: Source Reduction Activities---high wages to revenue ratio (labour intensive)
#======================================================================================================================#
sdid_source_reduction_hwrr <- fixest::feols(
  source.reduction ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_source_reduction_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_source_reduction_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material Substitution---high wages to revenue (wages to revenue ratio)
#======================================================================================================================#
sdid_mat_submod_hwrr <- fixest::feols(
  material.subandmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_submod_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_submod_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Organic Solvent Substitution---high wages ratio (wages to revenue ratio)
#======================================================================================================================#
sdid_organic_solvent_hwrr <- fixest::feols(
  sub.organic.solvent.matsubmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_organic_solvent_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_organic_solvent_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Feedstock and Reactive Chemical Substitution---high wage ratio (wages to revenue ratio)
#======================================================================================================================#
sdid_feedstock_reactchems_hwrr <- fixest::feols(
  sub.rawm.feedstock.reactchem.matsubmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_feedstock_reactchems_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_feedstock_reactchems_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: manufacturing, processing aids, and ancillary chemical Substitution---high wages ratio
#======================================================================================================================#
sdid_manu_aid_hwrr <- fixest::feols(
  sub.manu.proccess.ancilliary.chems.matsubmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_manu_aid_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_manu_aid_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Increase Purity of Chemicals---high wages to revenue ratio
#======================================================================================================================#
sdid_mat_purity_hwrr <- fixest::feols(
  mod.content.grade.purity.chems.matsubmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_purity_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_purity_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Clean Fuel---high wages to revenue ratio
#======================================================================================================================#
sdid_clean_fuel_hwrr <- fixest::feols(
  sub.fuel.matsubmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_clean_fuel_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_clean_fuel_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: Energy Cost Intensity---high wages to revenue ratio
#======================================================================================================================#
sdid_energy_intensity_hwrr <- fixest::feols(
  l.energy.cost.intensity ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_energy_intensity_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_energy_intensity_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Other material modification---high wages to revenue ratio
#======================================================================================================================#
sdid_mat_others_hwrr <- fixest::feols(
  other.matmods.matsubmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_others_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_others_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification---high wages to revenue ratio
#======================================================================================================================#
sdid_prod_mod_hwrr <- fixest::feols(
  product.modification ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  , data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_prod_mod_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_prod_mod_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: New Product Line---high wages to revenue ratio
#======================================================================================================================#
sdid_new_prod_hine_hwrr <- fixest::feols(
  devd.newproductline.pmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_new_prod_hine_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_new_prod_hine_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: Modified Packaging---high wages to revenue ratio
#======================================================================================================================#
sdid_mod_packaging_hwrr <- fixest::feols(
  mod.packaging.pmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mod_packaging_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mod_packaging_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: Other Product Modification---high wages to revenue ratio
#======================================================================================================================#
sdid_prod_mod_others_hwrr <- fixest::feols(
  other.pmods.pmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_prod_mod_others_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_prod_mod_others_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Process and Equipment Modification---high wages to revenue ratio
#======================================================================================================================#
sdid_process_equip_mod_hwrr <- fixest::feols(
  process.equip.modification ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_process_equip_mod_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_process_equip_mod_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Optimised Process Efficiency---high wages to revenue ratio
#======================================================================================================================#
sdid_optimised_process_efficiency_hwrr <- fixest::feols(
  optimised.process.efficiency.pequipmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_optimised_process_efficiency_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_optimised_process_efficiency_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Recirculation in Process---high wages to revenue ratio
#======================================================================================================================#
sdid_recirculate_inprocess_hwrr <- fixest::feols(
  recirculationinprocess.pequipmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recirculate_inprocess_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_recirculate_inprocess_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: New Technology and Technique in manufacturing process---high wages to revenue ratio
#======================================================================================================================#
sdid_new_tech_hwrr <- fixest::feols(
  newtech.technique.process.pequipmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_new_tech_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_new_tech_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Recycling to reuse---high wages to revenue ratio
#======================================================================================================================#
sdid_recylce_dummy_hwrr <- fixest::feols(
  recycling.dummy ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recylce_dummy_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_recylce_dummy_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Research & Development---high wages to revenue ratio
#======================================================================================================================#
sdid_research_hwrr <- fixest::feols(
  r.and.d ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_research_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_research_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Process Modification, Others---high wages to revenue ratio
#======================================================================================================================#
sdid_process_mod_others_hwrr <- fixest::feols(
  other.pequipmods.pequipmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_process_mod_others_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_process_mod_others_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Total factor productivity---high wages to revenue ratio
#======================================================================================================================#
sdid_tfp_hwrr <- fixest::feols(
  l.tfp5 ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_tfp_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_tfp_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Inventory Material Management---high wages to revenue ratio
#======================================================================================================================#
sdid_inventory_mat_mgt_hwrr <- fixest::feols(
  inventory.material.mgt ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_inventory_mat_mgt_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_inventory_mat_mgt_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Better Labelling and Testing---high wages to revenue ratio
#======================================================================================================================#
sdid_better_habelling_testing_hwrr <- fixest::feols(
  better.labelling.testing.immgt ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_better_habelling_testing_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_better_habelling_testing_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Containers size change---high wages to revenue ratio
#======================================================================================================================#
sdid_containers_sizechange_hwrr <- fixest::feols(
  containers.sizechange.immgt ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_containers_sizechange_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_containers_sizechange_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Improved material handling operations---high wages to revenue ratio
#======================================================================================================================#
sdid_mat_handling_hwrr <- fixest::feols(
  improved.materialhandling.operations.immgt ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_handling_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_handling_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Improved monitoring---high wages to revenue ratio
#======================================================================================================================#
sdid_improved_monitoring_hwrr <- fixest::feols(
  improved.monitoring.immgt ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_improved_monitoring_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_improved_monitoring_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Inventory Management, Others---high wages to revenue ratio
#======================================================================================================================#
sdid_inventory_mgt_others_hwrr <- fixest::feols(
  other.immgts.immgt ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_inventory_mgt_others_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_inventory_mgt_others_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Operating Practices & Training---high wages to revenue ratio
#======================================================================================================================#
sdid_operating_practices_training_hwrr <- fixest::feols(
  operating.practices.training ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_operating_practices_training_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_operating_practices_training_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Improved Scheduling Operation---high wages to revenue ratio
#======================================================================================================================#
sdid_improved_schedule_operation_hwrr <- fixest::feols(
  improved.schdule.operation.procedures.opt ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_improved_schedule_operation_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_improved_schedule_operation_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Changed Production Schedule---high wages to revenue ratio
#======================================================================================================================#
sdid_changed_prod_schedule_hwrr <- fixest::feols(
  changed.production.schedule.opt ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_changed_prod_schedule, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_changed_prod_schedule, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Inline Product Quality and Process Analysis---high wages to revenue ratio
#======================================================================================================================#
sdid_inline_prod_quality_analysis_hwrr <- fixest::feols(
  intro.inline.productquality.process.analysis.opt ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_inline_prod_quality_analysis_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_inline_prod_quality_analysis_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Inline Product Quality and Process Analysis---high wages to revenue ratio
#======================================================================================================================#
sdid_inline_prod_quality_analysis_hwrr <- fixest::feols(
  intro.inline.productquality.process.analysis.opt ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_inline_prod_quality_analysis_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_inline_prod_quality_analysis_hwrr, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#